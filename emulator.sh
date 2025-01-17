
SDK="$HOME/Downloads/android-sdk"

### boot emulator
AVD_NAME=$("${SDK}/emulator/emulator" -list-avds | head -n 1)
$SDK/emulator/emulator -avd "${AVD_NAME}" &
"${SDK}/platform-tools/adb" wait-for-device
"${SDK}/platform-tools/adb" logcat
