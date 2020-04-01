import ammonite.ops.ImplicitWd._
import ammonite.ops._

class KillAdbThread {
    // On kill it messes with current chosen device in Android Studio which is annoying
    val frequency = 15 * 60 * 1000L

    private def run() {
        while (true) {
            try {
                Thread.sleep(frequency)
            } catch {
                case _: InterruptedException =>
            }
            try {
                // sysinternals kill adb
                %("pskill", "adb")
            } catch {
                case e: Exception => e.printStackTrace()
            }
        }
    }

    def startInNewThread(): Unit = {
        val thread = new Thread(() => run())
        thread.start()
    }
}
