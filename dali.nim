{.experimental: "codeReordering".}

import random

include dex
import dex

# nimble install
# nim compile -f --run src/dali.nim

proc return_void*(): Instr = return newInstr(0x0e, RawXX(0))
proc const_string*(reg: uint8, s: String): Instr = return newInstr(0x1a, RegXX(reg), StringXXXX(s))
proc sget_object*(reg: uint8, field: Field): Instr = return newInstr(0x62, RegXX(reg), FieldXXXX(field))
proc invoke_virtual*(regC: dex.uint4, regD: dex.uint4, m: Method): Instr =
  return newInstr(0x6e, RawX(2), RawX(0), MethodXXXX(m), RawX(regD), RegX(regC), RawXX(0))
proc newInstr*(opcode: uint8, args: varargs[Arg]): Instr = return Instr(opcode: opcode, args: @args)

proc dehexify(s: string): string =
  result = newString(s.len div 2)
  for i in 0 ..< s.len div 2:
    let chunk = s.substr(2 * i, 2 * i + 1)
    if chunk[0] == '.':
      result[i] = chunk[1]
    else:
      result[i] = parseHexStr(chunk)[0]

let hello_world_apk = """
.d .e .x 0A .0 .3 .5 00 6F 53 89 BC 1E 79 B2 4F 1F 9C 09 66 15 23 2D 3B 56 65 32 C3 B5 81 B4 5A
70 02 00 00 70 00 00 00 78 56 34 12 00 00 00 00 00 00 00 00 DC 01 00 00 0C 00 00 00 70 00 00 00
07 00 00 00 A0 00 00 00 02 00 00 00 BC 00 00 00 01 00 00 00 D4 00 00 00 02 00 00 00 DC 00 00 00
01 00 00 00 EC 00 00 00 64 01 00 00 0C 01 00 00 A6 01 00 00 3A 01 00 00 8A 01 00 00 40 01 00 00
B4 01 00 00 76 01 00 00 54 01 00 00 6C 01 00 00 57 01 00 00 70 01 00 00 A1 01 00 00 C8 01 00 00
01 00 00 00 02 00 00 00 03 00 00 00 04 00 00 00 05 00 00 00 06 00 00 00 08 00 00 00 07 00 00 00
05 00 00 00 34 01 00 00 07 00 00 00 05 00 00 00 2C 01 00 00 04 00 01 00 0A 00 00 00 00 00 01 00
09 00 00 00 01 00 00 00 0B 00 00 00 00 00 00 00 01 00 00 00 02 00 00 00 00 00 00 00 FF FF FF FF
00 00 00 00 D1 01 00 00 00 00 00 00 02 00 01 00 02 00 00 00 00 00 00 00 08 00 00 00 62 00 00 00
1A 01 00 00 6E 20 01 00 10 00 0E 00 01 00 00 00 06 00 00 00 01 00 00 00 03 00 04 .L .h .w .; 00
12 .L .j .a .v .a ./ .l .a .n .g ./ .O .b .j .e .c .t .; 00 01 .V 00 13 .[ .L .j .a .v .a ./ .l
.a .n .g ./ .S .t .r .i .n .g .; 00 02 .V .L 00 04 .m .a .i .n 00 12 .L .j .a .v .a ./ .l .a .n
.g ./ .S .y .s .t .e .m .; 00 15 .L .j .a .v .a ./ .i .o ./ .P .r .i .n .t .S .t .r .e .a .m .;
00 03 .o .u .t 00 0C .H .e .l .l .o 20 .W .o .r .l .d .! 00 12 .L .j .a .v .a ./ .l .a .n .g ./
.S .t .r .i .n .g .; 00 07 .p .r .i .n .t .l .n 00 00 00 01 00 00 09 8C 02 00 00 00 0C 00 00 00
00 00 00 00 01 00 00 00 00 00 00 00 01 00 00 00 0C 00 00 00 70 00 00 00 02 00 00 00 07 00 00 00
A0 00 00 00 03 00 00 00 02 00 00 00 BC 00 00 00 04 00 00 00 01 00 00 00 D4 00 00 00 05 00 00 00
02 00 00 00 DC 00 00 00 06 00 00 00 01 00 00 00 EC 00 00 00 01 20 00 00 01 00 00 00 0C 01 00 00
01 10 00 00 02 00 00 00 2C 01 00 00 02 20 00 00 0C 00 00 00 3A 01 00 00 00 20 00 00 01 00 00 00
D1 01 00 00 00 10 00 00 01 00 00 00 DC 01 00 00
""".multiReplace(("\n", ""), (" ", "")).dehexify


let dex2 = newDex()
dex2.classes.add(ClassDef(
    class: "Lhw;",
    access: {Public},
    superclass: SomeType("Ljava/lang/Object;"),
    class_data: ClassData(
        direct_methods: @[
            EncodedMethod(
                m: Method(
                class: "Lhw;",
                name: "main",
                prototype: Prototype(ret: "V", params: @["[Ljava/lang/String;"])),
                access: {Public, Static},
                code: SomeCode(Code(
                registers: 2,
                ins: 1,
                outs: 2,
                instrs: @[
                    sget_object(0, Field(class: "Ljava/lang/System;", typ: "Ljava/io/PrintStream;", name: "out")),
                    const_string(1, "Hello World!"),
                    invoke_virtual(0, 1, Method(class: "Ljava/io/PrintStream;", name: "println",
                    prototype: Prototype(ret: "V", params: @["Ljava/lang/String;"]))),
                    return_void(),
                ]))
            )
        ]
    )
))

const HexChars = "0123456789ABCDEF"

func printable(c: char): bool =
  let n = ord(c)
  return 0x21 <= n and n <= 0x7E

proc dumpHex(s: string): string =
  if s.len == 0: return ""
  let nlines = (s.len + 15) div 16
  const
    left = 3*8 + 2 + 3*8 + 2
    right = 16
    line = left+right+1
  result = ' '.repeat(nlines*line)
  for i, ch in s:
    let
      y = i div 16
      xr = i mod 16
      xl = if xr < 8: 3*xr else: 3*xr + 1
      n = ord(ch)
    result[y*line + xl] = HexChars[n shr 4]
    result[y*line + xl + 1] = HexChars[n and 0x0F]
    result[y*line + left + xr - 1] = if printable(ch): ch else: '.'
    if xr == 0:
      result[y*line + left + right - 1] = '\n'
  result = "\n " & result

assert dex2.render.dumpHex == hello_world_apk.dumpHex
# echo dex2.render.dumpHex

randomize()
echo rand(100)
