
include dex
import dex
include hex

# nimble install
# nim compile -f --run nim/main.nim

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
