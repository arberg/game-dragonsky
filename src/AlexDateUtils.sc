
import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.time.LocalDate
import java.time.LocalTime
import java.util.Date

import java.time.Duration

object DateUtils {

    val TIME_FORMAT = "HH:mm:ss"
    val DATE_FORMAT = "yyyy.MM.dd_HH.mm.ss"
    val DATE_FORMAT_MILLI = "yyyy.MM.dd_HH.mm.ss.SSS"

//    def now(): Long = System.currentTimeMillis()

    def formatDate(d: Date, format: String): String = {
        val dateFormat = new SimpleDateFormat(format)
        dateFormat.format(d)
    }

    def getDateAsString(d: Date): String = {
        val dateFormat = new SimpleDateFormat(DATE_FORMAT)
        dateFormat.format(d)
    }

    // Midnight at 00:00, use .toEpochSecond(ZoneOffset.ofHours(0))
    def midnight(): LocalDateTime = LocalDateTime.of(LocalDate.now(), LocalTime.MIDNIGHT)

    def convertStringToDate(s: String): Date = {
        val dateFormat = new SimpleDateFormat(DATE_FORMAT)
        dateFormat.parse(s)
    }

    def getHourOfDay(): Int = LocalDateTime.now.getHour

    // 1-31
    def getDayOfMonth(): Int = LocalDateTime.now.getDayOfMonth

    def twoDigit(value: Long) = if (-10 < value && value < 10) "0" + value else value + ""

    def convertSecToString(sec: Long): String = twoDigit(sec / 3600) + ":" + twoDigit(sec / 60 % 60) + ":" + twoDigit(sec % 60)

    // Requires newer than java8, works on java13
    def convertDurationToString(duration: Duration): String = duration.toHours + ":" + duration.toMinutesPart + ":" + duration.toSecondsPart
}


