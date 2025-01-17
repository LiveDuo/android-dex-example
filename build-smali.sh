
SDK="$HOME/Downloads/android-sdk"

mkdir -p "/tmp/android-build"
BUILD_DIR=$(realpath "/tmp/android-build")

### create apk
ANDROID_JAR="$(ls -d ${SDK}/platforms/*/ | tail -n 1)/android.jar"
BUILD_TOOLS=$(ls -d ${SDK}/build-tools/*/ | tail -n 1)
java -jar ./smali/smali.jar assemble ./smali -o ${BUILD_DIR}/apk/classes.dex
"${BUILD_TOOLS}/aapt" package -f -M AndroidManifest.xml -I $ANDROID_JAR -F $BUILD_DIR/app.apk $BUILD_DIR/apk

### sign apk
[ ! -f "${BUILD_DIR}/keystore.jks" ] && keytool -genkeypair -keystore "${BUILD_DIR}/keystore.jks" \
    -dname "C=GB" -keyalg RSA -alias a -storepass android -keypass android
"${BUILD_TOOLS}/zipalign" -f -p 4 $BUILD_DIR/app.apk $BUILD_DIR/app.aligned.apk
"${BUILD_TOOLS}/apksigner" sign --ks "${BUILD_DIR}/keystore.jks" --ks-key-alias a \
    --ks-pass pass:android --key-pass pass:android --out $BUILD_DIR/app.signed.apk $BUILD_DIR/app.aligned.apk

### install apk
"${SDK}/platform-tools/adb" uninstall com.andreas.hello
"${SDK}/platform-tools/adb" install -r $BUILD_DIR/app.signed.apk
"${SDK}/platform-tools/adb" shell am start -n com.andreas.hello/.HelloAndroid
