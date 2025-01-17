
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
          registers: 4,
          ins: 2,
          outs: 2,
          instrs: @[
            invoke_super(2, 3, Method(class: "Landroid/app/Activity;", name: "onCreate",
              prototype: Prototype(ret: "V", params: @["Landroid/os/Bundle;"]))),
            new_instance(0, "Landroid/widget/TextView;"),
            invoke_direct(0, 2, Method(class: "Landroid/widget/TextView;", name: "<init>",
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
writeFile("/tmp/android-build/apk/classes.dex", dex2.render)
