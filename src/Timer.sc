import $file.AlexDateUtils
import AlexDateUtils._
import upickle.default.{macroRW, ReadWriter => RW}

case class SimpleTimer() {
    val startTime: Long = System.currentTimeMillis()

    def timePassed() = System.currentTimeMillis() - startTime
}

case class MyTimer(defaultDurationSec: Long = 0, info: String = "", var nextDurationSec: Long = 0, var startTime: Long = 0, var isRunning: Boolean = false) {
    private def currentTimeSec() = System.currentTimeMillis() / 1000

    def copyActiveStateFrom(other: MyTimer) {
        startTime = other.startTime
        isRunning = other.isRunning
        if (other.nextDurationSec != 0) nextDurationSec = other.nextDurationSec
    }

    def setNextDuration(setNextDurationSec: Long): MyTimer = {
        if (isRunning) {
            startTime = currentTimeSec() - (this.nextDurationSec - setNextDurationSec)
        } else {
            this.nextDurationSec = setNextDurationSec
        }
        this
    }

    def start() = {
        startTime = currentTimeSec()
        isRunning = true
        // never overwrite nextDurationSec on start, so just set it if its not set
        if (nextDurationSec == 0) nextDurationSec = defaultDurationSec
        this
    }

    /* maintains the first initial base time, so screenshots should be regular from the start */
    /** resets timer and starts a new loop based on the start of the previous loop. */
    def startNextLoop() = {
        if (defaultDurationSec <= 0) throw new IllegalStateException("0 duration not allowed " + defaultDurationSec)
        val currentTime = currentTimeSec()
        startTime += nextDurationSec
        nextDurationSec = defaultDurationSec
        while (startTime > currentTime) {
            startTime += defaultDurationSec
        }
    }

    def stopAndReset() = {
        startTime = 0
        isRunning = false
        nextDurationSec = defaultDurationSec
        this
    }

    def restart() {
        nextDurationSec = defaultDurationSec
        start
    }

    def checkRunningOrStart() {
        if (!isRunning) start
    }

    def isTimePassed() = isRunning && delta > nextDurationSec

    def isTimePassedWithRestart() = {
        val timePassed = isTimePassed()
        if (timePassed) {
            println(s"Timer $info")
            start()
        }
        timePassed
    }

    def delta(): Long = currentTimeSec() - startTime

    def deltaDuration(): String = {
        DateUtils.convertSecToString(delta)
    }

    def remainingToString(): String = {
        //            val remaining = Duration.ofSeconds(durationSec - delta)
        //            DateUtils.convertDurationToString(remaining)
        DateUtils.convertSecToString(if (isRunning) nextDurationSec - delta else 0)
    }

    def nextDurationToString(): String = {
        //            val remaining = Duration.ofSeconds(durationSec - delta)
        //            DateUtils.convertDurationToString(remaining)
        DateUtils.convertSecToString(if (isRunning) nextDurationSec - delta else nextDurationSec)
    }

    def remainingOrNextSec(): Long = {
        if (isRunning) {
            (nextDurationSec - delta)
        } else {
            nextDurationSec
        }
    }

    def remainingOrNextMin(): Long = {
        remainingOrNextSec() / 60
    }
}

object MyTimer {
    implicit val rw: RW[MyTimer] = macroRW
}