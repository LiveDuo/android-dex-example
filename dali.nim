
include dex
import dex
include hex

# nimble install
# nim compile -f --run dali.nim

proc newInstr*(opcode: uint8, args: varargs[Arg]): Instr = return Instr(opcode: opcode, args: @args)

proc return_void*(): Instr = return newInstr(0x0e, RawXX(0))
proc const_string*(reg: uint8, s: String): Instr = return newInstr(0x1a, RegXX(reg), StringXXXX(s))
proc sget_object*(reg: uint8, field: Field): Instr = return newInstr(0x62, RegXX(reg), FieldXXXX(field))
proc invoke_virtual*(regC: dex.uint4, regD: dex.uint4, m: Method): Instr =
  return newInstr(0x6e, RawX(2), RawX(0), MethodXXXX(m), RawX(regD), RegX(regC), RawXX(0))

proc newInvoke1*(opcode: uint8, regC: uint4, m: Method): Instr =
  return newInstr(opcode, RawX(1), RawX(0), MethodXXXX(m), RawX(0), RegX(regC), RawXX(0))

proc invoke_direct*(regC: uint4, m: Method): Instr =
  return newInvoke1(0x70, regC, m)

proc newInvoke2*(opcode: uint8, regC: uint4, regD: uint4, m: Method): Instr =
  return newInstr(opcode, RawX(2), RawX(0), MethodXXXX(m), RawX(regD), RegX(regC), RawXX(0))

proc invoke_super*(regC: uint4, regD: uint4, m: Method): Instr =
  return newInvoke2(0x6f, regC, regD, m)

proc const_high16*(reg: uint8, highBits: uint16): Instr =
  return newInstr(0x15, RegXX(reg), RawXXXX(highBits))

let dex2 = newDex()
#-- Prime some arrays, to make sure their order matches hello_android_apk
dex2.addStr"<init>"
dex2.addStr"I"
dex2.addStr"Landroid/app/Activity;"
dex2.addStr"Landroid/os/Bundle;"
dex2.addStr"Lcom/akavel/hello/HelloAndroid;"
dex2.addStr"V"
dex2.addStr"VI"
dex2.addStr"VL"
dex2.addStr"onCreate"
dex2.addStr"setContentView"
dex2.addTypeList(@["I"])

dex2.classes.add(ClassDef(
  class: "Lcom/akavel/hello/HelloAndroid;",
  access: {Public},
  superclass: SomeType("Landroid/app/Activity;"),
  class_data: ClassData(
    direct_methods: @[
      EncodedMethod(
        m: Method(
          class: "Lcom/akavel/hello/HelloAndroid;",
          name: "<init>",
          prototype: Prototype(ret: "V", params: @[]),
        ),
        access: {Public, Constructor},
        code: SomeCode(Code(
          registers: 1,
          ins: 1,
          outs: 1,
          instrs: @[
            invoke_direct(0, Method(class: "Landroid/app/Activity;", name: "<init>",
              prototype: Prototype(ret: "V", params: @[]))),
            return_void(),
          ],
        )),
      ),
    ],
    virtual_methods: @[
      EncodedMethod(
        m: Method(
          class: "Lcom/akavel/hello/HelloAndroid;",
          name: "onCreate",
          prototype: Prototype(
            ret: "V",
            params: @["Landroid/os/Bundle;"],
          ),
        ),
        access: {Public},
        code: SomeCode(Code(
          registers: 3,
          ins: 2,
          outs: 2,
          instrs: @[
            invoke_super(1, 2, Method(class: "Landroid/app/Activity;", name: "onCreate",
              prototype: Prototype(ret: "V", params: @["Landroid/os/Bundle;"]))),
            const_high16(0, 0x7f03),
            invoke_virtual(1, 0, Method(class: "Lcom/akavel/hello/HelloAndroid;", name: "setContentView",
              prototype: Prototype(ret: "V", params: @["I"]))),
            return_void(),
          ],
        )),
      ),
    ],
  )
))
writeFile("classes.dex", dex2.render)

let hello_world_apk = """
6465 780a 3033 3500 2f4f 153b 3623 8747
6d02 4697 5b1e 959d a8b1 2f0f 9c3a a14f
7802 0000 7000 0000 7856 3412 0000 0000
0000 0000 f001 0000 0a00 0000 7000 0000
0500 0000 9800 0000 0300 0000 ac00 0000
0000 0000 0000 0000 0500 0000 d000 0000
0100 0000 f800 0000 6001 0000 1801 0000
6201 0000 6a01 0000 6d01 0000 8501 0000
9a01 0000 bc01 0000 bf01 0000 c301 0000
c701 0000 d101 0000 0100 0000 0200 0000
0300 0000 0400 0000 0500 0000 0500 0000
0400 0000 0000 0000 0600 0000 0400 0000
5401 0000 0700 0000 0400 0000 5c01 0000
0100 0000 0000 0000 0100 0200 0800 0000
0300 0000 0000 0000 0300 0200 0800 0000
0300 0100 0900 0000 0300 0000 0100 0000
0100 0000 0000 0000 ffff ffff 0000 0000
e101 0000 0000 0000 0100 0100 0100 0000
0000 0000 0400 0000 7010 0000 0000 0e00
0300 0200 0200 0000 0000 0000 0900 0000
6f20 0100 2100 1500 037f 6e20 0400 0100
0e00 0000 0100 0000 0000 0000 0100 0000
0200 063c 696e 6974 3e00 0149 0016 4c61
6e64 726f 6964 2f61 7070 2f41 6374 6976
6974 793b 0013 4c61 6e64 726f 6964 2f6f
732f 4275 6e64 6c65 3b00 204c 636f 6d2f
616e 6472 6f69 642f 6865 6c6c 6f2f 4865
6c6c 6f41 6e64 726f 6964 3b00 0156 0002
5649 0002 564c 0008 6f6e 4372 6561 7465
000e 7365 7443 6f6e 7465 6e74 5669 6577
0000 0001 0102 8180 0498 0203 01b0 0200
0b00 0000 0000 0000 0100 0000 0000 0000
0100 0000 0a00 0000 7000 0000 0200 0000
0500 0000 9800 0000 0300 0000 0300 0000
ac00 0000 0500 0000 0500 0000 d000 0000
0600 0000 0100 0000 f800 0000 0120 0000
0200 0000 1801 0000 0110 0000 0200 0000
5401 0000 0220 0000 0a00 0000 6201 0000
0020 0000 0100 0000 e101 0000 0010 0000
0100 0000 f001 0000
""".multiReplace(("\n", ""), (" ", "")).dehexify

# assert dex2.render.dumpHex == hello_world_apk.dumpHex
