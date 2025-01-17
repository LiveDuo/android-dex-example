.class public Lcom/andreas/hello/HelloAndroid;
.super Landroid/app/Activity;


.method public constructor <init>()V
    .registers 1 # p0
    invoke-direct {p0}, Landroid/app/Activity;-><init>()V
    return-void
.end method

.method protected onCreate(Landroid/os/Bundle;)V
    .registers 3 # p0, p1 & v0

    invoke-super {p0, p1}, Landroid/app/Activity;->onCreate(Landroid/os/Bundle;)V

    # new text
    new-instance p1, Landroid/widget/TextView;
    invoke-direct {p1, p0}, Landroid/widget/TextView;-><init>(Landroid/content/Context;)V

    # set text
    const-string v0, "hello"
    invoke-virtual {p1, v0}, Landroid/widget/TextView;->setText(Ljava/lang/CharSequence;)V

    # set content view
    invoke-virtual {p0, p1}, Lcom/andreas/hello/HelloAndroid;->setContentView(Landroid/view/View;)V

    return-void
.end method
