
import critbits, strutils, sequtils, std/sha1, tables, hashes, bitops

####################################
##### Blob
####################################

type
  Blob = distinct string
  uint4 = range[0..15]
  Slot32 = distinct int

  Slots32[T] = distinct TSlots32[T]
  TSlots32[T] = Table[T, seq[Slot32]]

template `>>:`(slot32: Slot32, slot: untyped): untyped =
  let slot = slot32

proc `>>`(slot32: Slot32, slot: var Slot32) =
  slot = slot32

proc skip(b: var Blob, n: int) {.inline.} =
  let pos = b.string.len
  b.string.setLen(pos + n)
  for i in pos ..< b.string.len:
    b.string[i] = chr(0)

proc slot32(b: var Blob): Slot32 {.inline.} =
  result = b.string.len.Slot32
  b.skip(4)

proc set(b: var Blob, slot: Slot32, v: uint32) =
  let i = slot.int
  # Little-endian
  b.string[i+0] = chr(v and 0xff)
  b.string[i+1] = chr(v shr 8 and 0xff)
  b.string[i+2] = chr(v shr 16 and 0xff)
  b.string[i+3] = chr(v shr 24 and 0xff)

proc `[]=`(b: var Blob, slot: Slot32, v: uint32) =
  b.set(slot, v)

proc pad32(b: var Blob) {.inline.} =
  let n = (4 - (b.string.len mod 4)) mod 4
  b.skip(n)

proc puts(b: var Blob, v: string) =
  var s = b.string
  let pos = s.len
  s.setLen(pos + v.len)
  copyMem(addr(s[pos]), cstring(v), v.len)
  b = s.Blob

proc putc(b: var Blob, v: char) {.inline.} =
  b.puts($v)

proc put32(b: var Blob, v: uint32) =
  b.set(b.slot32, v)

proc put32(b: var Blob): Slot32 =
  b.slot32()

proc put16(b: var Blob, v: uint16) =
  # Little-endian
  var buf = newString(2)
  buf[0] = chr(v and 0xff)
  buf[1] = chr(v shr 8 and 0xff)
  b.puts(buf)

## (https://source.android.com/devices/tech/dalvik/dex-format#leb128)
proc put_uleb128(b: var Blob, v: uint32) =
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

proc put4(b: var Blob, v: uint4, high: bool) =
  var s = b.string
  let pos = s.len
  if high:
    s.setLen(pos + 1)
    s[pos] = chr(v.uint8 shl 4)
  else:
    s[pos-1] = chr(s[pos-1].ord.uint8 or v.uint8)
  b = s.Blob

proc pos(b: var Blob): uint32 {.inline.} =
  return b.string.len.uint32

proc add[T](slots: var Slots32[T], key: T, val: Slot32) =
  slots.TSlots32[:T].mgetOrPut(key, newSeq[Slot32]()).add(val)

proc setAll[T](slots: Slots32[T], key: T, val: uint32, blob: var Blob) =
  if slots.TSlots32[:T].contains(key):
    for slot in slots.TSlots32[:T][key]: blob.set(slot, val)



####################################
##### Sorted set
####################################

type SortedSet[T] = distinct seq[T]

proc incl[T](s: var SortedSet[T], item: T) =
  let i = s.search(item)
  if i == s.len:
    seq[T](s).add(item)
  elif item < s[i]:
    seq[T](s).insert(item, i)

proc `[]`[T](s: SortedSet[T], i: int): T {.inline.} = seq[T](s)[i]

proc len[T](s: SortedSet[T]): int {.inline.} = seq[T](s).len

proc search[T](s: SortedSet[T], item: T): int =
  var i = s.len
  while result < i:
    let mid = (result + i) shr 1
    if item < s[mid]:
      i = mid
    elif s[mid] < item:
      result = mid + 1
    else:
      return mid

iterator items[T](s: SortedSet[T]): T =
  for item in seq[T](s):
    yield item


####################################
##### Types
####################################

type
  Field = ref object
    class: Type
    typ: Type
    name: String
  Type = String
  String = string
  Method = ref object
    class: Type
    prototype: Prototype  # a.k.a. method signature
    name: String
  Prototype = ref object
    ret: Type
    params: TypeList
  TypeList = seq[Type]

type
  ArgKind* {.pure.} = enum RawX, RawXX, RawXXXX, RegX, RegXX, FieldXXXX, StringXXXX, TypeXXXX, MethodXXXX
  Arg = object
    case kind: ArgKind
    of ArgKind.RawX: raw4*: uint4
    of ArgKind.RawXX: raw8*: uint8
    of ArgKind.RawXXXX: raw16*: uint16
    of ArgKind.RegX: reg4*: uint4
    of ArgKind.RegXX: reg8*: uint8
    of ArgKind.FieldXXXX: field16*: Field
    of ArgKind.StringXXXX: string16*: String
    of ArgKind.TypeXXXX: type16*: Type
    of ArgKind.MethodXXXX: method16*: Method

proc RawX*(raw4: uint4): Arg {.used.} = Arg(kind: ArgKind.RawX, raw4: raw4)
proc RawXX*(raw8: uint8): Arg {.used.} = Arg(kind: ArgKind.RawXX, raw8: raw8)
proc RawXXXX*(raw16: uint16): Arg {.used.} = Arg(kind: ArgKind.RawXXXX, raw16: raw16)
proc RegX*(reg4: uint4): Arg {.used.} = Arg(kind: ArgKind.RegX, reg4: reg4)
proc RegXX*(reg8: uint8): Arg {.used.} = Arg(kind: ArgKind.RegXX, reg8: reg8)
proc FieldXXXX*(field16: Field): Arg {.used.} = Arg(kind: ArgKind.FieldXXXX, field16: field16)
proc StringXXXX*(string16: String): Arg {.used.} = Arg(kind: ArgKind.StringXXXX, string16: string16)
proc TypeXXXX*(type16: Type): Arg {.used.} = Arg(kind: ArgKind.TypeXXXX, type16: type16)
proc MethodXXXX*(method16: Method): Arg {.used.} = Arg(kind: ArgKind.MethodXXXX, method16: method16)




type
  Instr = ref object
    opcode: uint8
    args: seq[Arg]
  Code = ref object
    registers: uint16
    ins: uint16
    outs: uint16
    instrs: seq[Instr]




type
  MaybeTypeKind* {.pure.} = enum SomeType, NoType
  MaybeType = object
    case kind: MaybeTypeKind
    of MaybeTypeKind.SomeType: typ*: Type
    of MaybeTypeKind.NoType: nil

proc SomeType*(typ: Type): MaybeType {.used.} = MaybeType(kind: MaybeTypeKind.SomeType, typ: typ)
proc NoType*(): MaybeType {.used.} = MaybeType(kind: MaybeTypeKind.NoType)


type
  MaybeCodeKind* {.pure.} = enum SomeCode, NoCode
  MaybeCode = object
    case kind: MaybeCodeKind
    of MaybeCodeKind.SomeCode: code*: Code
    of MaybeCodeKind.NoCode: nil

proc SomeCode*(code: Code): MaybeCode {.used.} = MaybeCode(kind: MaybeCodeKind.SomeCode, code: code)
proc NoCode*(): MaybeCode {.used.} = MaybeCode(kind: MaybeCodeKind.NoCode)


type
  ClassDef = ref object
    class: Type
    access: set[Access]
    superclass: MaybeType
    class_data: ClassData
  ClassData = ref object
    instance_fields: seq[EncodedField]
    direct_methods: seq[EncodedMethod]
    virtual_methods: seq[EncodedMethod]
  EncodedField = ref object
    f: Field
    access: set[Access]
  EncodedMethod = ref object
    m: Method
    access: set[Access]
    code: MaybeCode
  Access = enum
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

type
  Dex = ref object
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
    classes: seq[ClassDef]


####################################
##### Instructions
####################################

const
  # OP_MOVE_RESULT = 0x0c
  OP_RETURN_VOID = 0x0e
  # OP_RETURN_OBJECT = 0x11
  # OP_CONST_HIGH_16 = 0x15
  # OP_CONST_WIDE_16 = 0x16
  OP_CONST_STRING = 0x1a
  OP_NEW_INSTANCE = 0x22
  # OP_GET_WIDE = 0x53
  # OP_PUT_WIDE = 0x5a
  # OP_SGET_OBJECT = 0x62
  OP_INVOKE_VIRTUAL = 0x6e
  OP_INVOKE_SUPER = 0x6f
  OP_INVOKE_DIRECT = 0x70
  # OP_INVOKE_STATIC = 0x71

# proc move_result_object(reg: uint8): Instr =
#   return Instr(opcode: OP_MOVE_RESULT, args: @[RegXX(reg)])

# proc return_object(reg: uint8): Instr =
#   return Instr(opcode: OP_RETURN_OBJECT, args: @[RawXX(reg)])

# proc const_high16(reg: uint8, highBits: uint16): Instr =
#   return Instr(opcode: OP_CONST_HIGH_16, args: @[RegXX(reg), RawXXXX(highBits)])

# proc const_wide_16(regs: uint8, v: int16): Instr =
#   return Instr(opcode: OP_CONST_WIDE_16, args: @[RegXX(regs), RawXXXX(v.uint16)])

# proc get_wide(regsA: uint4, regB: uint4, field: Field): Instr =
#   return Instr(opcode: OP_GET_WIDE, args: @[RegX(regB), RegX(regsA), FieldXXXX(field)])

# proc put_wide(regsA: uint4, regB: uint4, field: Field): Instr =
#   return Instr(opcode: OP_PUT_WIDE, args: @[RegX(regB), RegX(regsA), FieldXXXX(field)])

# proc sget_object(reg: uint8, field: Field): Instr =
#   return Instr(opcode: OP_SGET_OBJECT, args: @[RegXX(reg), FieldXXXX(field)])

# proc invoke_static(regC: uint4, regD: uint4, m: Method): Instr =
#   return Instr(opcode: OP_INVOKE_STATIC, args: @[RawX(2), RawX(0), MethodXXXX(m), RawX(regD), RegX(regC), RawXX(0)])

proc return_void(): Instr =
  return Instr(opcode: OP_RETURN_VOID, args: @[RawXX(0)])

proc const_string(reg: uint8, s: String): Instr =
  return Instr(opcode: OP_CONST_STRING, args: @[RegXX(reg), StringXXXX(s)])

proc new_instance(reg: uint8, t: Type): Instr =
  return Instr(opcode: OP_NEW_INSTANCE, args: @[RegXX(reg), TypeXXXX(t)])

proc invoke_virtual(regC: uint4, regD: uint4, m: Method): Instr =
  return Instr(opcode: OP_INVOKE_VIRTUAL, args: @[RawX(2), RawX(0), MethodXXXX(m), RawX(regD), RegX(regC), RawXX(0)])

proc invoke_super(regC: uint4, regD: uint4, m: Method): Instr =
  return Instr(opcode: OP_INVOKE_SUPER, args: @[RawX(2), RawX(0), MethodXXXX(m), RawX(regD), RegX(regC), RawXX(0)])

proc invoke_direct_1(regC: uint4, regD: uint4, m: Method): Instr =
  return Instr(opcode: OP_INVOKE_DIRECT, args: @[RawX(1), RawX(0), MethodXXXX(m), RawX(regD), RegX(regC), RawXX(0)])

proc invoke_direct_2(regC: uint4, regD: uint4, m: Method): Instr =
  return Instr(opcode: OP_INVOKE_DIRECT, args: @[RawX(2), RawX(0), MethodXXXX(m), RawX(regD), RegX(regC), RawXX(0)])


####################################
##### Helper functions
####################################

proc hash(proto: Prototype): Hash =
  var h: Hash = 0
  h = h !& hash(proto.ret)
  h = h !& hash(proto.params)
  result = !$h

proc `<`(p1, p2: Prototype): bool =
  if p1.ret != p2.ret:
    return p1.ret < p2.ret
  for i in 0 ..< min(p1.params.len, p2.params.len):
    if p1.params[i] != p2.params[i]:
      return p1.params[i] < p2.params[i]
  return p1.params.len < p2.params.len

proc descriptor(proto: Prototype): string =
  proc typeChar(t: Type): string =
    if t.len==1 and t[0] in {'V','Z','B','S','C','I','J','F','D'}:
      return t
    elif t.len>=1 and t[0] in {'[','L'}:
      return "L"
  return (proto.ret & proto.params).map(typeChar).join


# https://en.wikipedia.org/wiki/Adler-32
proc adler32(s: string): uint32 =
  var a: uint32 = 1
  var b: uint32 = 0
  const MOD_ADLER = 65521
  for c in s:
    a = (a + c.uint32) mod MOD_ADLER
    b = (b + a) mod MOD_ADLER
  result = (b shl 16) or a

proc addField(dex: Dex, f: Field) =

  discard dex.strings.containsOrIncl(f.class, dex.strings.len)
  dex.types.incl(f.class)

  discard dex.strings.containsOrIncl(f.typ, dex.strings.len)
  dex.types.incl(f.typ)

  discard dex.strings.containsOrIncl(f.name, dex.strings.len)
  dex.fields.incl((f.class, f.name, f.typ))

proc addMethod(dex: Dex, m: Method) =
  discard dex.strings.containsOrIncl(m.class, dex.strings.len)
  dex.types.incl(m.class)

  # prototype
  discard dex.strings.containsOrIncl(m.prototype.ret, dex.strings.len)
  dex.types.incl(m.prototype.ret)
  for t in m.prototype.params:
    discard dex.strings.containsOrIncl(t, dex.strings.len)
    dex.types.incl(t)
  if m.prototype.params notin dex.typeLists:
    dex.typeLists.add(m.prototype.params)
  dex.prototypes.incl(m.prototype)
  discard dex.strings.containsOrIncl(m.prototype.descriptor, dex.strings.len)

  discard dex.strings.containsOrIncl(m.name, dex.strings.len)
  dex.methods.incl((m.class, m.name, m.prototype))




####################################
##### Render function
####################################

proc render(dex: Dex): string =

  # Collect strings and all the things from classes.
  # (types, prototypes/signatures, fields, methods)
  for c in dex.classes:
    discard dex.strings.containsOrIncl(c.class, dex.strings.len)
    dex.types.incl(c.class)
    if c.superclass.kind == MaybeTypeKind.SomeType:
      discard dex.strings.containsOrIncl(c.superclass.typ, dex.strings.len)
      dex.types.incl(c.superclass.typ)
    let cd = c.class_data
    for f in cd.instance_fields:
      dex.addField(f.f)
    for dm in cd.direct_methods & cd.virtual_methods:
      dex.addMethod(dm.m)
      if dm.code.kind == MaybeCodeKind.SomeCode:
        for instr in dm.code.code.instrs:
          for arg in instr.args:
            case arg.kind
            of RawX:
              discard
            of RawXX:
              discard
            of RawXXXX:
              discard
            of RegX:
              discard
            of RegXX:
              discard
            of FieldXXXX:
              dex.addField(arg.field16)
            of StringXXXX:
              discard dex.strings.containsOrIncl(arg.string16, dex.strings.len)
            of TypeXXXX:
              discard dex.strings.containsOrIncl(arg.type16, dex.strings.len)
              dex.types.incl(arg.type16)
            of MethodXXXX:
              dex.addMethod(arg.method16)

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

  echo "header"
  let bytes = cast[seq[byte]](blob.string)
  echo bytes.map(proc(c: byte): string = c.ord.toHex(2)).join(" ")

  #-- Partially render string_ids
  # We preallocate space for the list of string offsets. We cannot fill it yet, as its contents
  # will depend on the size of the other segments.
  sections.add (0x0001'u16, blob.pos, dex.strings.len)
  blob[slots.stringIdsOff] = blob.pos
  for i in 0 ..< dex.strings.len:
    blob.put32 >> slots.stringOffsets[i]

  #-- Render typeIDs.
  sections.add (0x0002'u16, blob.pos, dex.types.len)
  blob[slots.typeIdsOff] = blob.pos

  var stringIds = newSeq[int](dex.strings.len)
  var i = 0
  for s, added in dex.strings:
    stringIds[added] = i
    inc i

  for t in dex.types:
    blob.put32 stringIds[dex.strings[t]].uint32

  #-- Partially render proto IDs.
  sections.add (0x0003'u16, blob.pos, dex.prototypes.len)
  blob[slots.protoIdsOff] = blob.pos
  var typeListOffsets: Slots32[seq[Type]]
  for p in dex.prototypes:
    blob.put32 stringIds[dex.strings[p.descriptor]].uint32
    blob.put32 dex.types.search(p.ret).uint32
    blob.put32 >>: slot
    typeListOffsets.add(p.params, slot)

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
    blob.put32 c.access.foldl(a or b.ord.uint32, 0'u32)

    case c.superclass.kind
    of SomeType:
      blob.put32 dex.types.search(c.superclass.typ).uint32
    of NoType:
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
        let tupl = (class: dm.m.class, name: dm.m.name, proto: dm.m.prototype)
        codeOffsets[tupl] = blob.pos
        blob.put16 code.registers
        blob.put16 code.ins
        blob.put16 code.outs
        blob.put16 0'u16   # TODO: tries_size
        blob.put32 0'u32   # TODO: debug_info_off
        blob.put32 >>: slot   # This shall be filled with size of instrs, in 16-bit code units

        var
          high = true
        for instr in code.instrs:
          blob.putc instr.opcode.chr
          for arg in instr.args:

            case arg.kind
            of RawX:
              blob.put4 arg.raw4, high
              high = not high
            of RawXX:
              blob.putc arg.raw8.chr
            of RawXXXX:
              blob.put16 arg.raw16
            of RegX:
              blob.put4 arg.reg4, high
              high = not high
            of RegXX:
              blob.putc arg.reg8.chr
            of FieldXXXX:
              let v = arg.field16
              blob.put16 dex.fields.search((v.class, v.name, v.typ)).uint16
            of StringXXXX:
              blob.put16 stringIds[dex.strings[arg.string16]].uint16
            of TypeXXXX:
              blob.put16 dex.types.search(arg.type16).uint16
            of MethodXXXX:
              let v = arg.method16
              blob.put16 dex.methods.search((v.class, v.name, v.prototype)).uint16

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

  var stringSequence = newSeq[string](dex.strings.len)
  for s, added in dex.strings:
    stringSequence[added] = s

  for s in stringSequence:
    let slot = slots.stringOffsets[stringIds[dex.strings[s]]]
    blob[slot] = blob.pos
    blob.put_uleb128 s.len.uint32
    blob.puts s & "\x00"

  #-- Render class data
  sections.add (0x2000'u16, blob.pos, dex.classes.len)
  for c in dex.classes:
    classDataOffsets.setAll(c.class, blob.pos, blob)
    let d = c.class_data
    blob.put_uleb128 0
    blob.put_uleb128 d.instance_fields.len.uint32
    blob.put_uleb128 d.direct_methods.len.uint32
    blob.put_uleb128 d.virtual_methods.len.uint32

    # renderEncodedFields
    var prev = 0
    for f in d.instance_fields:
      let tupl = (class: f.f.class, name: f.f.name, typ: f.f.typ)
      let idx = dex.fields.search(tupl)
      blob.put_uleb128 uint32(idx - prev)
      prev = idx
      blob.put_uleb128 f.access.foldl(a or b.ord.uint32, 0'u32)

    # renderEncodedMethods (direct)
    var prev2 = 0
    for m in d.direct_methods:
      let tupl = (class: m.m.class, name: m.m.name, proto: m.m.prototype)
      let idx = dex.methods.search(tupl)
      blob.put_uleb128 uint32(idx - prev2)
      prev2 = idx
      blob.put_uleb128 m.access.foldl(a or b.ord.uint32, 0'u32)
      if Native notin m.access and Abstract notin m.access:
        blob.put_uleb128 codeOffsets[tupl]
      else:
        blob.put_uleb128 0

    # renderEncodedMethods (virtual)
    var prev3 = 0
    for m in d.virtual_methods:
      let tupl = (class: m.m.class, name: m.m.name, proto: m.m.prototype)
      let idx = dex.methods.search(tupl)
      blob.put_uleb128 uint32(idx - prev3)
      prev3 = idx
      blob.put_uleb128 m.access.foldl(a or b.ord.uint32, 0'u32)
      if Native notin m.access and Abstract notin m.access:
        blob.put_uleb128 codeOffsets[tupl]
      else:
        blob.put_uleb128 0

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


####################################
##### Create .dex
####################################

let dexData = new(Dex)
dexData.classes.add(ClassDef(
  class: "Lcom/andreas/hello/HelloAndroid;",
  access: {Public},
  superclass: SomeType("Landroid/app/Activity;"),
  class_data: ClassData(
    direct_methods: @[
      EncodedMethod(
        m: Method(
          class: "Lcom/andreas/hello/HelloAndroid;",
          name: "<init>",
          prototype: Prototype(ret: "V", params: @[]),
        ),
        access: {Public, Constructor},
        code: SomeCode(Code(
          registers: 1,
          ins: 1,
          outs: 1,
          instrs: @[
            invoke_direct_1(0, 0, Method(class: "Landroid/app/Activity;", name: "<init>",
              prototype: Prototype(ret: "V", params: @[]))),
            return_void(),
          ],
        )),
      ),
    ],
    virtual_methods: @[
      EncodedMethod(
        m: Method(
          class: "Lcom/andreas/hello/HelloAndroid;",
          name: "onCreate",
          prototype: Prototype(ret: "V", params: @["Landroid/os/Bundle;"]),
        ),
        access: {Public},
        code: SomeCode(Code(
          registers: 4,
          ins: 2,
          outs: 2,
          instrs: @[
            invoke_super(2, 3, Method(class: "Landroid/app/Activity;", name: "onCreate",
              prototype: Prototype(ret: "V", params: @["Landroid/os/Bundle;"]))),
            new_instance(0, "Landroid/widget/TextView;"),
            invoke_direct_2(0, 2, Method(class: "Landroid/widget/TextView;", name: "<init>",
              prototype: Prototype(ret: "V", params: @["Landroid/content/Context;"]))),
            const_string(1, "Hello"),
            invoke_virtual(0, 1, Method(class: "Landroid/widget/TextView;", name: "setText",
              prototype: Prototype(ret: "V", params: @["Ljava/lang/CharSequence;"]))),
            invoke_virtual(2, 0, Method(class: "Lcom/andreas/hello/HelloAndroid;", name: "setContentView",
              prototype: Prototype(ret: "V", params: @["Landroid/view/View;"]))),
            return_void(),
          ],
        )),
      ),
    ],
  )
))

writeFile("/tmp/android-build/apk/classes.dex", dexData.render)
