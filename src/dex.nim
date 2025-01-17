
import critbits, strutils, sequtils, std/sha1, tables, hashes, bitops

import patty


# Potentially useful bibliography
#
# DEX:
# - https://github.com/corkami/pics/blob/master/binary/DalvikEXecutable.pdf
# - https://source.android.com/devices/tech/dalvik/dex-format
# - https://blog.bugsnag.com/dex-and-d8/
# - http://benlynn.blogspot.com/2009/02/minimal-dalvik-executables_06.html
#
# APK:
# - https://fractalwrench.co.uk/posts/playing-apk-golf-how-low-can-an-android-app-go/
# - https://github.com/fractalwrench/ApkGolf
#
# Opcodes:
# - https://github.com/corkami/pics/blob/master/binary/opcodes_tables_compact.pdf
# - https://source.android.com/devices/tech/dalvik/dalvik-bytecode.html
#
# MORE:
# - https://github.com/JesusFreke/smali
# - https://github.com/linkedin/dexmaker
# - https://github.com/iBotPeaches/Apktool




type
  Blob* = distinct string
  uint4* = range[0..15]   # "nibble" / hex digit / half-byte
  Slot32* = distinct int
  # Slot16* = distinct int

  Slots32*[T] = distinct TSlots32[T]
  TSlots32[T] = Table[T, seq[Slot32]]

proc skip*(b: var Blob, n: int) {.inline.} =
  let pos = b.string.len
  b.string.setLen(pos + n)
  for i in pos ..< b.string.len:
    b.string[i] = chr(0)

template `>>:`*(slot32: Slot32, slot: untyped): untyped =
  let slot = slot32

proc `>>`*(slot32: Slot32, slot: var Slot32) =
  slot = slot32

proc slot32*(b: var Blob): Slot32 {.inline.} =
  result = b.string.len.Slot32
  b.skip(4)

proc set*(b: var Blob, slot: Slot32, v: uint32) =
  let i = slot.int
  # Little-endian
  b.string[i+0] = chr(v and 0xff)
  b.string[i+1] = chr(v shr 8 and 0xff)
  b.string[i+2] = chr(v shr 16 and 0xff)
  b.string[i+3] = chr(v shr 24 and 0xff)

proc `[]=`*(b: var Blob, slot: Slot32, v: uint32) =
  b.set(slot, v)

proc pad32*(b: var Blob) {.inline.} =
  let n = (4 - (b.string.len mod 4)) mod 4
  b.skip(n)

proc puts*(b: var Blob, v: string) =
  if v.len == 0:
    return
  var s = b.string
  let pos = s.len
  s.setLen(pos + v.len)
  copyMem(addr(s[pos]), cstring(v), v.len)
  b = s.Blob

proc putc*(b: var Blob, v: char) {.inline.} =
  b.puts($v)

proc put32*(b: var Blob, v: uint32) =
  b.set(b.slot32, v)

proc put32*(b: var Blob): Slot32 =
  b.slot32()

proc put16*(b: var Blob, v: uint16) =
  # Little-endian
  var buf = newString(2)
  buf[0] = chr(v and 0xff)
  buf[1] = chr(v shr 8 and 0xff)
  b.puts(buf)

proc put_uleb128*(b: var Blob, v: uint32) =
  ## Writes an uint32 in ULEB128 format
  ## (https://source.android.com/devices/tech/dalvik/dex-format#leb128)
  if v == 0:
    b.puts("\x00")
    return
  let
    topBit = fastLog2(v)  # position of the highest bit set
    n = topBit div 7 + 1  # number of bytes required for ULEB128 encoding of 'v'
  var
    buf = newString(n.Natural)
    work = v
    i = 0
  while work >= 0x80'u32:
    buf[i] = chr(0x80.byte or (work and 0x7F).byte)
    work = work shr 7
    inc i
  buf[i] = chr(work.byte)
  b.puts(buf)

proc put4*(b: var Blob, v: uint4, high: bool) =
  var s = b.string
  let pos = s.len
  if high:
    s.setLen(pos + 1)
    s[pos] = chr(v.uint8 shl 4)
  else:
    s[pos-1] = chr(s[pos-1].ord.uint8 or v.uint8)
  b = s.Blob

proc pos*(b: var Blob): uint32 {.inline.} =
  return b.string.len.uint32


proc add*[T](slots: var Slots32[T], key: T, val: Slot32) =
  slots.TSlots32[:T].mgetOrPut(key, newSeq[Slot32]()).add(val)

proc setAll*[T](slots: Slots32[T], key: T, val: uint32, blob: var Blob) =
  if not slots.TSlots32[:T].contains(key): return
  for slot in slots.TSlots32[:T][key]: blob.set(slot, val)


type
  Field* = ref object
    class*: Type
    typ*: Type
    name*: String
  Type* = String
  String* = string
  Method* = ref object
    class*: Type
    prototype*: Prototype  # a.k.a. method signature
    name*: String
  Prototype* = ref object
    ret*: Type
    params*: TypeList
  TypeList* = seq[Type]



variantp Arg:  # Argument of an instruction of Dalvik bytecode
  RawX(raw4: uint4)
  RawXX(raw8: uint8)
  RawXXXX(raw16: uint16)
  RegX(reg4: uint4)
  RegXX(reg8: uint8)
  FieldXXXX(field16: Field)
  StringXXXX(string16: String)
  TypeXXXX(type16: Type)
  MethodXXXX(method16: Method)




type
  Instr* = ref object
    opcode*: uint8
    args*: seq[Arg]
  Code* = ref object
    registers*: uint16
    ins*: uint16
    outs*: uint16 # "the number of words of outgoing argument space required by this code for method invocation"
    instrs*: seq[Instr]



variantp MaybeType:
  SomeType(typ: Type)
  NoType

variantp MaybeCode:
  SomeCode(code: Code)
  NoCode


type
  ClassDef* = ref object
    class*: Type
    access*: set[Access]
    superclass*: MaybeType
    class_data*: ClassData
  ClassData* = ref object
    instance_fields*: seq[EncodedField]
    direct_methods*: seq[EncodedMethod]
    virtual_methods*: seq[EncodedMethod]
  EncodedField* = ref object
    f*: Field
    access*: set[Access]
  EncodedMethod* = ref object
    m*: Method
    access*: set[Access]
    code*: MaybeCode
  Access* = enum
    Public = 0x1
    Private = 0x2
    Protected = 0x4
    Static = 0x8
    Final = 0x10
    Synchronized = 0x20
    Varargs = 0x80
    Native = 0x100
    Interface = 0x200
    Abstract = 0x400
    Annotation = 0x2000
    Enum = 0x4000
    Constructor = 0x1_0000

  NotImplementedYetError* = object of CatchableError
  ConsistencyError* = object of CatchableError

proc hash*(proto: Prototype): Hash =
  var h: Hash = 0
  h = h !& hash(proto.ret)
  h = h !& hash(proto.params)
  result = !$h
func equals[T](a, b: seq[T]): bool =
  if a.len != b.len: return false
  for i in 0..<a.len:
    if not a[i].equals(b[i]): return false
  return true





type SortedSet*[T] = distinct seq[T]

proc init*[T](s: var SortedSet[T]) {.inline.} = s = SortedSet[T](newSeq[T]())

proc incl*[T](s: var SortedSet[T], item: T) =
  let i = s.search(item)
  if i == s.len:
    seq[T](s).add(item)
  elif item < s[i]:
    seq[T](s).insert(item, i)

proc `[]`*[T](s: SortedSet[T], i: int): T {.inline.} = seq[T](s)[i]

proc len*[T](s: SortedSet[T]): int {.inline.} = seq[T](s).len

proc search*[T](s: SortedSet[T], item: T): int =
  var i = s.len
  while result < i:
    let mid = (result + i) shr 1
    if item < s[mid]:
      i = mid
    elif s[mid] < item:
      result = mid + 1
    else:
      return mid

iterator items*[T](s: SortedSet[T]): T =
  for item in seq[T](s):
    yield item



type
  Dex* = ref object
    # Note: below fields are generally ordered from simplest to more complex
    # (in order of dependency)
    strings: CritBitTree[int]  # value: order of addition
    types: SortedSet[string]
    typeLists: seq[seq[Type]]
    # NOTE: prototypes must have no duplicates, TODO: and be sorted by:
    # (ret's type ID; args' type ID)
    prototypes: SortedSet[Prototype]
    # NOTE: fields must have no duplicates, TODO: and be sorted by:
    # (class type ID, field name's string ID, field's type ID)
    fields: SortedSet[tuple[class: Type, name: string, typ: Type]]
    # NOTE: methods must have no duplicates, TODO: and be sorted by:
    # (class type ID, name's string ID, prototype's proto ID)
    methods: SortedSet[tuple[class: Type, name: string, proto: Prototype]]
    classes*: seq[ClassDef]


proc newDex*(): Dex = new(result)

proc `<`(p1, p2: Prototype): bool =
  # echo "called <"
  if p1.ret != p2.ret:
    return p1.ret < p2.ret
  for i in 0 ..< min(p1.params.len, p2.params.len):
    if p1.params[i] != p2.params[i]:
      return p1.params[i] < p2.params[i]
  return p1.params.len < p2.params.len
converter toUint32[T: enum](s: set[T]): uint32 =
  for v in s:
    result = result or v.ord.uint32
#########
#########


proc descriptor(proto: Prototype): string =
  proc typeChar(t: Type): string =
    if t.len==1 and t[0] in {'V','Z','B','S','C','I','J','F','D'}:
      return t
    elif t.len>=1 and t[0] in {'[','L'}:
      return "L"
    else:
      raise newException(ConsistencyError, "unexpected type in prototype: " & t)

  return (proto.ret & proto.params).map(typeChar).join


proc addStr(dex: Dex, s: string) =
  if s.contains({'\x00', '\x80'..'\xFF'}):
    raise newException(NotImplementedYetError, "strings with 0x00 or 0x80..0xFF bytes are not yet supported")
  discard dex.strings.containsOrIncl(s, dex.strings.len)
  # "This list must be sorted by string contents, using UTF-16 code point
  # values (not in a locale-sensitive manner), and it must not contain any
  # duplicate entries." [dex-format] <- I think this is guaranteed by UTF-8 + CritBitTree type


proc addType(dex: Dex, t: Type) =
  dex.addStr(t)
  dex.types.incl(t)


proc addField(dex: Dex, f: Field) =
  dex.addType(f.class)
  dex.addType(f.typ)
  dex.addStr(f.name)
  dex.fields.incl((f.class, f.name, f.typ))

proc addTypeList(dex: Dex, ts: seq[Type]) =
  if ts.len == 0:
    return
  for t in ts:
    dex.addType(t)
  if ts notin dex.typeLists:
    dex.typeLists.add(ts)

proc addPrototype(dex: Dex, proto: Prototype) =
  dex.addType(proto.ret)
  dex.addTypeList(proto.params)
  dex.prototypes.incl(proto)
  dex.addStr(proto.descriptor)

proc addMethod(dex: Dex, m: Method) =
  dex.addType(m.class)
  dex.addPrototype(m.prototype)
  dex.addStr(m.name)
  dex.methods.incl((m.class, m.name, m.prototype))







proc stringsOrdering(dex: Dex): seq[int] =
  var i = 0
  result.setLen dex.strings.len
  for s, added in dex.strings:
    result[added] = i
    inc i

proc stringsAsAdded(dex: Dex): seq[string] =
  result.setLen dex.strings.len
  for s, added in dex.strings:
    result[added] = s

func asTuple(f: Field): tuple[class: Type, name: string, typ: Type] =
  return (class: f.class, name: f.name, typ: f.typ)
func asTuple(m: Method): tuple[class: Type, name: string, proto: Prototype] =
  return (class: m.class, name: m.name, proto: m.prototype)

proc adler32(s: string): uint32 =
  # https://en.wikipedia.org/wiki/Adler-32
  var a: uint32 = 1
  var b: uint32 = 0
  const MOD_ADLER = 65521
  for c in s:
    a = (a + c.uint32) mod MOD_ADLER
    b = (b + a) mod MOD_ADLER
  result = (b shl 16) or a

proc renderInstrs(dex: Dex, blob: var Blob, instrs: openArray[Instr], stringIds: openArray[int]) =
  var
    high = true
  for instr in instrs:
    blob.putc instr.opcode.chr
    for arg in instr.args:
      # FIXME(akavel): padding
      match arg:
        RawX(v):
          blob.put4 v, high
          high = not high
        RawXX(v):
          blob.putc v.chr
        RawXXXX(v):
          blob.put16 v
        RegX(v):
          blob.put4 v, high
          high = not high
        RegXX(v):
          blob.putc v.chr
        FieldXXXX(v):
          blob.put16 dex.fields.search((v.class, v.name, v.typ)).uint16
        StringXXXX(v):
          blob.put16 stringIds[dex.strings[v]].uint16
        TypeXXXX(v):
          blob.put16 dex.types.search(v).uint16
        MethodXXXX(v):
          blob.put16 dex.methods.search((v.class, v.name, v.prototype)).uint16


proc collect(dex: Dex) =
  # Collect strings and all the things from classes.
  # (types, prototypes/signatures, fields, methods)
  for c in dex.classes:
    dex.addType(c.class)
    if c.superclass.kind == MaybeTypeKind.SomeType:
      dex.addType(c.superclass.typ)
    let cd = c.class_data
    for f in cd.instance_fields:
      dex.addField(f.f)
    for dm in cd.direct_methods & cd.virtual_methods:
      dex.addMethod(dm.m)
      if dm.code.kind == MaybeCodeKind.SomeCode:
        for instr in dm.code.code.instrs:
          for arg in instr.args:
            match arg:
              RawX(_): discard
              RawXX(_): discard
              RawXXXX(_): discard
              RegX(_): discard
              RegXX(_): discard
              FieldXXXX(f):
                dex.addField(f)
              StringXXXX(s):
                dex.addStr(s)
              TypeXXXX(t):
                dex.addType(t)
              MethodXXXX(m):
                dex.addMethod(m)

proc renderEncodedFields(dex: Dex, blob: var Blob, fields: openArray[EncodedField]) =
  var prev = 0
  for f in fields:
    let tupl = f.f.asTuple
    let idx = dex.fields.search(tupl)
    blob.put_uleb128 uint32(idx - prev)
    prev = idx
    blob.put_uleb128 f.access.toUint32

proc renderEncodedMethods(dex: Dex, blob: var Blob, methods: openArray[EncodedMethod], codeOffsets: Table[tuple[class: Type, name: string, proto: Prototype], uint32]) =
  var prev = 0
  for m in methods:
    let tupl = m.m.asTuple
    let idx = dex.methods.search(tupl)
    blob.put_uleb128 uint32(idx - prev)
    prev = idx
    blob.put_uleb128 m.access.toUint32
    if Native notin m.access and Abstract notin m.access:
      blob.put_uleb128 codeOffsets[tupl]
    else:
      blob.put_uleb128 0


proc render*(dex: Dex): string =
  # stderr.write(dex.repr)
  dex.collect()

  # Storage for offsets where various sections of the file
  # start. Will be needed to render map_list.
  # NOTE: n is number of elements in the section, not length in bytes.
  var sections: seq[tuple[kind: uint16, pos: uint32, n: int]]

  # blob is the buffer where we will render the binary contents of the .dex file
  var blob: Blob

  # slots are places in the blob, where we can't put data immediately. That's
  # because the value that should be there depends on some data further along
  # in the file. We bookmark some space for them in the blob, for filling in
  # later, when we will know what to put in the slot.
  var slots: tuple[
    adlerSum: Slot32,
    fileSize: Slot32,
    mapOffset: Slot32,
    stringIdsOff: Slot32,
    typeIdsOff: Slot32,
    protoIdsOff: Slot32,
    fieldIdsOff: Slot32,
    methodIdsOff: Slot32,
    classDefsOff: Slot32,
    dataSize: Slot32,
    dataOff: Slot32,
    stringOffsets: seq[Slot32],
  ]
  slots.stringOffsets.setLen(dex.strings.len)

  # FIXME: ensure correct padding everywhere

  #-- Partially render header
  # Most of it can only be calculated after the rest of the segments.
  sections.add (0x0000'u16, blob.pos, 1)
  # TODO: handle various versions of targetSdkVersion file, not only 035
  blob.puts  "dex\n035\x00"       # Magic prefix
  blob.put32 >> slots.adlerSum
  blob.skip(20)                   # SHA1 sum; we will fill it much later
  blob.put32 >> slots.fileSize
  blob.put32 0x70                 # Header size
  blob.put32 0x12345678   # Endian constant
  blob.put32 0            # link_size
  blob.put32 0            # link_off
  blob.put32 >> slots.mapOffset
  blob.put32 dex.strings.len.uint32
  blob.put32 >> slots.stringIdsOff
  blob.put32 dex.types.len.uint32
  blob.put32 >> slots.typeIdsOff
  blob.put32 dex.prototypes.len.uint32
  blob.put32 >> slots.protoIdsOff
  blob.put32 dex.fields.len.uint32
  blob.put32 >> slots.fieldIdsOff
  blob.put32 dex.methods.len.uint32
  blob.put32 >> slots.methodIdsOff
  blob.put32 dex.classes.len.uint32
  blob.put32 >> slots.classDefsOff
  blob.put32 >> slots.dataSize
  blob.put32 >> slots.dataOff

  # stderr.write(blob.string.dumpHex)
  # stderr.write("\n")

  # blob.reserve(0x70 - blob.pos.int)

  #-- Partially render string_ids
  # We preallocate space for the list of string offsets. We cannot fill it yet, as its contents
  # will depend on the size of the other segments.
  sections.add (0x0001'u16, blob.pos, dex.strings.len)
  blob[slots.stringIdsOff] = blob.pos
  for i in 0 ..< dex.strings.len:
    blob.put32 >> slots.stringOffsets[i]

  # stderr.write(blob.string.dumpHex)
  # stderr.write("\n")

  #-- Render typeIDs.
  sections.add (0x0002'u16, blob.pos, dex.types.len)
  blob[slots.typeIdsOff] = blob.pos
  let stringIds = dex.stringsOrdering
  # dex.types are already stored sorted, same as dex.strings, so we don't need
  # to sort again by type IDs
  for t in dex.types:
    blob.put32 stringIds[dex.strings[t]].uint32

  #-- Partially render proto IDs.
  # We cannot fill offsets for parameters (type lists), as they'll depend on the size of the
  # segments inbetween.
  sections.add (0x0003'u16, blob.pos, dex.prototypes.len)
  blob[slots.protoIdsOff] = blob.pos
  var typeListOffsets: Slots32[seq[Type]]
  for p in dex.prototypes:
    blob.put32 stringIds[dex.strings[p.descriptor]].uint32
    blob.put32 dex.types.search(p.ret).uint32
    blob.put32 >>: slot
    typeListOffsets.add(p.params, slot)
    # echo p.ret, " ", p.params

  #-- Render field IDs
  if dex.fields.len > 0:
    sections.add (0x0004'u16, blob.pos, dex.fields.len)
    blob[slots.fieldIdsOff] = blob.pos
  for f in dex.fields:
    blob.put16 dex.types.search(f.class).uint16
    blob.put16 dex.types.search(f.typ).uint16
    blob.put32 stringIds[dex.strings[f.name]].uint32

  #-- Render method IDs
  sections.add (0x0005'u16, blob.pos, dex.methods.len)
  if dex.methods.len > 0:
    blob[slots.methodIdsOff] = blob.pos
  for m in dex.methods:
    # echo $m
    blob.put16 dex.types.search(m.class).uint16
    blob.put16 dex.prototypes.search(m.proto).uint16
    blob.put32 stringIds[dex.strings[m.name]].uint32

  #-- Partially render class defs.
  sections.add (0x0006'u16, blob.pos, dex.classes.len)
  blob[slots.classDefsOff] = blob.pos
  var classDataOffsets: Slots32[Type]
  const NO_INDEX = 0xffff_ffff'u32
  for c in dex.classes:
    blob.put32 dex.types.search(c.class).uint32
    blob.put32 c.access.uint32
    match c.superclass:
      SomeType(t):
        blob.put32 dex.types.search(t).uint32
      NoType:
        blob.put32 NO_INDEX
    blob.put32 0'u32      # TODO: interfaces_off
    blob.put32 NO_INDEX   # TODO: source_file_idx
    blob.put32 0'u32      # TODO: annotations_off
    blob.put32 >>: slot
    classDataOffsets.add(c.class, slot)
    blob.put32 0'u32      # TODO: static_values

  #-- Render code items
  let dataStart = blob.pos
  blob[slots.dataOff] = dataStart
  var
    codeItems = 0
    codeOffsets: Table[tuple[class: Type, name: string, proto: Prototype], uint32]
  for c in dex.classes:
    let cd = c.class_data
    for dm in cd.direct_methods & cd.virtual_methods:
      if dm.code.kind == MaybeCodeKind.SomeCode:
        codeItems.inc()
        let code = dm.code.code
        blob.pad32()
        codeOffsets[dm.m.asTuple] = blob.pos
        blob.put16 code.registers
        blob.put16 code.ins
        blob.put16 code.outs
        blob.put16 0'u16   # TODO: tries_size
        blob.put32 0'u32   # TODO: debug_info_off
        blob.put32 >>: slot   # This shall be filled with size of instrs, in 16-bit code units
        dex.renderInstrs(blob, code.instrs, stringIds)
        blob[slot] = (blob.pos - slot.uint32 - 4) div 2
  if codeItems > 0:
    sections.add (0x2001'u16, dataStart, codeItems)

  #-- Render type lists
  blob.pad32()
  if dex.typeLists.len > 0:
    sections.add (0x1001'u16, blob.pos, dex.typeLists.len)
  for l in dex.typeLists:
    blob.pad32()
    typeListOffsets.setAll(l, blob.pos, blob)
    blob.put32 l.len.uint32
    for t in l:
      blob.put16 dex.types.search(t).uint16

  #-- Render strings data
  sections.add (0x2002'u16, blob.pos, dex.strings.len)
  for s in dex.stringsAsAdded:
    let slot = slots.stringOffsets[stringIds[dex.strings[s]]]
    blob[slot] = blob.pos
    # FIXME: MUTF-8: encode U+0000 as hex: C0 80
    # FIXME: MUTF-8: use CESU-8 to encode code-points from beneath Basic Multilingual Plane (> U+FFFF)
    # FIXME: length *in UTF-16 code units*, as ULEB128
    blob.put_uleb128 s.len.uint32
    blob.puts s & "\x00"

  #-- Render class data
  sections.add (0x2000'u16, blob.pos, dex.classes.len)
  for c in dex.classes:
    classDataOffsets.setAll(c.class, blob.pos, blob)
    let d = c.class_data
    blob.put_uleb128 0  # TODO: static_fields_size
    blob.put_uleb128 d.instance_fields.len.uint32
    blob.put_uleb128 d.direct_methods.len.uint32
    blob.put_uleb128 d.virtual_methods.len.uint32
    # TODO: static_fields
    dex.renderEncodedFields(blob, d.instance_fields)
    dex.renderEncodedMethods(blob, d.direct_methods, codeOffsets)
    dex.renderEncodedMethods(blob, d.virtual_methods, codeOffsets)

  #-- Render map_list
  blob.pad32()
  sections.add (0x1000'u16, blob.pos, 1)
  blob[slots.mapOffset] = blob.pos
  blob.put32 sections.len.uint32
  for s in sections:
    blob.put16 s.kind
    blob.skip(2)   # unused
    blob.put32 s.n.uint32
    blob.put32 s.pos

  #-- Fill remaining slots related to file size
  blob[slots.dataSize] = blob.pos - dataStart  # FIXME: round to 64?
  blob[slots.fileSize] = blob.pos
  #-- Fill checksums
  let sha1 = secureHash(blob.string.substr(0x20)).Sha1Digest
  for i in 0 ..< 20:
    blob.string[0x0c + i] = sha1[i].char
  blob[slots.adlerSum] = adler32(blob.string.substr(0x0c))

  return blob.string



proc newInstr*(opcode: uint8, args: varargs[Arg]): Instr =
  return Instr(opcode: opcode, args: @args)

proc newInvoke1*(opcode: uint8, regC: uint4, m: Method): Instr =
  return newInstr(opcode, RawX(1), RawX(0), MethodXXXX(m), RawX(0), RegX(regC), RawXX(0))
proc newInvoke2*(opcode: uint8, regC: uint4, regD: uint4, m: Method): Instr =
  return newInstr(opcode, RawX(2), RawX(0), MethodXXXX(m), RawX(regD), RegX(regC), RawXX(0))

proc move_result_object*(reg: uint8): Instr =
  return newInstr(0x0c, RegXX(reg))

proc return_void*(): Instr =
  return newInstr(0x0e, RawXX(0))
proc return_object*(reg: uint8): Instr =
  return newInstr(0x11, RawXX(reg))

proc const_high16*(reg: uint8, highBits: uint16): Instr =
  return newInstr(0x15, RegXX(reg), RawXXXX(highBits))
proc const_wide_16*(regs: uint8, v: int16): Instr =
  return newInstr(0x16, RegXX(regs), RawXXXX(v.uint16))
proc const_string*(reg: uint8, s: String): Instr =
  return newInstr(0x1a, RegXX(reg), StringXXXX(s))

proc new_instance*(reg: uint8, t: Type): Instr =
  return newInstr(0x22, RegXX(reg), TypeXXXX(t))

proc iget_wide*(regsA: uint4, regB: uint4, field: Field): Instr =
  return newInstr(0x53, RegX(regB), RegX(regsA), FieldXXXX(field))
proc iput_wide*(regsA: uint4, regB: uint4, field: Field): Instr =
  return newInstr(0x5a, RegX(regB), RegX(regsA), FieldXXXX(field))

proc sget_object*(reg: uint8, field: Field): Instr =
  return newInstr(0x62, RegXX(reg), FieldXXXX(field))

proc invoke_virtual*(regC: uint4, m: Method): Instr =
  return newInvoke1(0x6e, regC, m)
proc invoke_virtual*(regC: uint4, regD: uint4, m: Method): Instr =
  return newInvoke2(0x6e, regC, regD, m)

proc invoke_super*(regC: uint4, regD: uint4, m: Method): Instr =
  return newInvoke2(0x6f, regC, regD, m)

proc invoke_direct*(regC: uint4, m: Method): Instr =
  return newInvoke1(0x70, regC, m)
proc invoke_direct*(regC: uint4, regD: uint4, m: Method): Instr =
  return newInvoke2(0x70, regC, regD, m)

proc invoke_static*(regC: uint4, m: Method): Instr =
  return newInvoke1(0x71, regC, m)
proc invoke_static*(regC, regD: uint4, m: Method): Instr =
  return newInvoke2(0x71, regC, regD, m)
