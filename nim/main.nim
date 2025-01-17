
include dex
import dex
include hex

# nimble install
# nim compile -f --run nim/main.nim

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
dex2.addStr"Lcom/andreas/hello/HelloAndroid;"
dex2.addStr"V"
dex2.addStr"VI"
dex2.addStr"VL"
dex2.addStr"onCreate"
dex2.addStr"setContentView"
dex2.addTypeList(@["I"])

dex2.classes.add(ClassDef(
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
          class: "Lcom/andreas/hello/HelloAndroid;",
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
            # const_high16(0, 0x7f03),
            # invoke_virtual(1, 0, Method(class: "Lcom/andreas/hello/HelloAndroid;", name: "setContentView",
            #   prototype: Prototype(ret: "V", params: @["I"]))),
            return_void(),
          ],
        )),
      ),
    ],
  )
))
writeFile("/tmp/android-build/apk/classes.dex", dex2.render)
