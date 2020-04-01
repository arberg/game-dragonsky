// What am I: Send touch and drags to android device or android emulator (such as bluestacks) playing the game DragonSky. Does not auto-play watch ads nor Legion.
// * Autoplays merge dragons (avoid merging shinies with regular blacks), pvp, daily bosses+journey+legendary, DimensionGate, search essenses and buy gold+free, search dragons (stops when hitting
// elite)
// * Enable auto-play pvp in the late evening, though it messes with return, so only if return is not near in time.
// * Auto returns, after predined X+Y amount of time. Rushes when done playing for X minutes, and then plays 'final stages' for Y minutes, then returns.
// * Control chosen stage in PlayerConfig. See estimated stages to complete before return in gui.
// Very brief pointers
// * See DeviceSerial.playerAndName for filtering of devices, and mapping deviceId's to PlayerConfig.
// * Run with http://ammonite.io/
// * 1) Download amm: https://github.com/lihaoyi/Ammonite/releases/download/2.0.4/2.12-2.0.4
// * 2) On windows rename to amm.cmd, and run 'amm gameDragonSkyAutoPlay.sc gui'. Primarily developed on ammonite 2.13-2.0.1 and seems to run on 2.13-2.0.4

// * Ammonite uses Java, and I use a few Java 13.0.2 (or at least newer than Java 8) classes, so it probably won't run on Java 8.

// TODO Click top right corner of ad every minute, to close ad. No don't work unless I add button that tells ad is in progress, or auto-click ad, which is too much work I think.
// Simple solution: Add button for stop/start where it starts in ad-click mode. Not worth much though.

// Run in REPL with :  amm --no-home-predef --predef .\gameDragonSkyAutoPlay.sc
//import ammonite.ops.ImplicitWd._
//import ammonite.ops._

import java.time.ZoneOffset
import java.util.concurrent.atomic.AtomicBoolean

import $file.AtomicRangedInteger, AtomicRangedInteger._
import $file.AlexDateUtils, AlexDateUtils._
import $file.KillAdbThread, KillAdbThread._
import $file.Timer, Timer._

import ammonite.ops.ImplicitWd._
import ammonite.ops._
import upickle.default.{macroRW, ReadWriter => RW}

//import $ivy.`com.google.code.gson:gson:2.8.6`
//import com.google.code.gson._

import java.time.LocalDateTime
import java.util.Date

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

// adb shell wm size 1080x1920
// adb shell wm density 480 <-- This value can vary per phone, you'll know it's right when you can read text and apps don't render too small value is from 120 up to 640
//
// adb shell wm size 720x1280
// adb shell wm density 350


val ENABLE_KNOWN_DEVICE_FILTER = false
val LOG_DRAW_CORNERS = false
val LOG_FILE_DATA = true
val DRY_RUN = false
val doMysticTapAfterMergeDragons = true // I have seen mystic legendary without tap thingy, but there's still sometimes button in the center
val doLogActions = true // logs drag, and taps with context info // TODO Only log maya
val doLogAllPlayers = true
val doLogAdb = false
val doLogAdbWithSerial = false
val dirScreenShots = Path("""d:\Users\Alex\Pictures\BlueStacks""")
val TIMER_START_AUTO_PLAYING_MS: Int = 15 * 60 * 1000
val MIN_PR_HOUR = 60
val SEC_PR_MIN = 60
val HOUR_PR_SEC = 3600
val MILLI_PR_SEC = 1000

val doThrottleSleep = false // Not with mystic hack
val throttleSleep = 200
val sleepDurationMenu: Int = 500
val sleepDurationMenuShort: Int = 250
val hourToDoDailyJourneyHour: Int = 21
// dateFunctions.sc

val r = new Random()


object StageType extends Enumeration {
    val Neutral = Value(0)
    val Fire = Value(1)
    val Water = Value(2)
    val Wood = Value(3)
    val valuesList = values.toList

    def next(value: Value) = valuesList((value.id + 1) % values.size)
}

val Alex = "Alex"
val Maya = "Maya"
val lock = new Object()

// I cannot figure out how to serialize non-case class with uPickle nor enumerations. I suspect it does not support it, see Limitations http://www.lihaoyi.com/upickle/
case class PlayerConfigJson(name: String,
                            rushThenReturnTimer: MyTimer,
                            finalStagesTimer: MyTimer,
                            screenShotTimer: MyTimer,
                            autoPlaySearchDimensionGate: MyTimer,
                            autoPlaySearchDragons: MyTimer,
                            autoPlayDailyBoss: MyTimer,
                            autoPlayLegendaryBoss: MyTimer,
                            autoPlayPvp: MyTimer,
                            autoPlayJourneyLastDayCompleted: Long,
                            autoPlayBattleLastDayTryUncompleted: Long,
                            nextStageOverwritten: Option[Int],
                            currentStageType: Option[Int],
                            nextRushCount: Option[Int]
                           )

object PlayerConfigJson {
    def create(p: PlayerConfig) = PlayerConfigJson(name = p.name,
        rushThenReturnTimer = p.rushThenReturnTimer,
        finalStagesTimer = p.finalStagesTimer,
        screenShotTimer = p.screenShotTimer,
        autoPlaySearchDimensionGate = p.autoPlaySearchDimensionGate,
        autoPlaySearchDragons = p.autoPlaySearchDragons,
        autoPlayDailyBoss = p.autoPlayDailyBoss,
        autoPlayLegendaryBoss = p.autoPlayLegendaryBoss,
        autoPlayPvp = p.autoPlayPvp,
        autoPlayJourneyLastDayCompleted = p.autoPlayJourneyLastDayCompleted,
        autoPlayBattleLastDayTryUncompleted = p.autoPlayBattleLastDayTryUncompleted,
        nextStageOverwritten = p.nextStageOverwritten.map(_.id),
        currentStageType = p.currentStageType.map(_.id),
        nextRushCount = p.nextRushCount
    )

    def updatePlayerConfig(pJson: PlayerConfigJson, player: PlayerConfig): PlayerConfig = {
        if (pJson.name != player.name) throw new RuntimeException("Name mismatch in loaded data " + pJson.name + ", " + player.name)
        player.rushThenReturnTimer.copyActiveStateFrom(pJson.rushThenReturnTimer)
        player.finalStagesTimer.copyActiveStateFrom(pJson.finalStagesTimer)
        player.screenShotTimer.copyActiveStateFrom(pJson.screenShotTimer)
        player.autoPlaySearchDimensionGate.copyActiveStateFrom(pJson.autoPlaySearchDimensionGate)
        player.autoPlaySearchDragons.copyActiveStateFrom(pJson.autoPlaySearchDragons)
        player.autoPlayDailyBoss.copyActiveStateFrom(pJson.autoPlayDailyBoss)
        player.autoPlayLegendaryBoss.copyActiveStateFrom(pJson.autoPlayLegendaryBoss)
        player.autoPlayPvp.copyActiveStateFrom(pJson.autoPlayPvp)
        player.autoPlayJourneyLastDayCompleted = pJson.autoPlayJourneyLastDayCompleted
        player.autoPlayBattleLastDayTryUncompleted = pJson.autoPlayBattleLastDayTryUncompleted
        player.nextStageOverwritten = pJson.nextStageOverwritten.map(StageType.valuesList(_))
        player.currentStageType = pJson.currentStageType.map(StageType.valuesList(_))
        player.nextRushCount = pJson.nextRushCount
        if (LOG_FILE_DATA) println(s"${player.name} json rush ${pJson.rushThenReturnTimer}")
        if (LOG_FILE_DATA) println(s"${player.name} done rush ${player.rushThenReturnTimer}")
        if (LOG_FILE_DATA) println(s"${player.name} json screenshot ${pJson.screenShotTimer}")
        if (LOG_FILE_DATA) println(s"${player.name} done screenshot ${player.screenShotTimer}")
        if (LOG_FILE_DATA) println(s"${player.name} json final ${pJson.finalStagesTimer}")
        if (LOG_FILE_DATA) println(s"${player.name} done final ${player.finalStagesTimer} - ${player.finalStagesTimer.remainingOrNextMin()}")
        player
    }

    implicit val rw: RW[PlayerConfigJson] = macroRW
}


val rushAdditionalPct = .50f // Current data of 246 rush's suggests X additional stages gives X*61% on average
case class PlayerConfig(
                           name: String,
                           playStageDuration: Int,
                           playStageDurationExtra: StageType.Value => Int = _ => 0,
                           finalStagesDuration: Int,
                           finalStagesDurationExtra: StageType.Value => Int = _ => 0,
                           rushStagesRandom: Int,
                           rushStagesFixed: Int,
                           rushCount: Int,
                           extraRushCount: Int = 0,
                           maxRushCount: Int = 6,
                           dailyBossDoMax30: Boolean = false,
                           var nextRushCount: Option[Int] = None,
                           selectStage: () => StageType.Value = () => StageType.Neutral,
                           doLog: Boolean = doLogAllPlayers
                       ) {
    var pvpAutoActive = false

    def playStagesDurationForLevel(stage: StageType.Value) = playStageDuration + playStageDurationExtra(stage)

    def finalStagesDurationForLevel(stage: StageType.Value) = finalStagesDuration + finalStagesDurationExtra(stage)

    val stagesPrRush = (rushStagesFixed + rushStagesRandom * rushAdditionalPct).toInt
    val rushThenReturnTimer = MyTimer(playStageDuration).start()
    val finalStagesTimer = MyTimer(finalStagesDuration).stopAndReset() // After Rush, before return
    var nextStageOverwritten: Option[StageType.Value] = None
    var currentStageType: Option[StageType.Value] = None // The stage I think we are currently playing
    val screenShotTimer = MyTimer(15 * SEC_PR_MIN).start()
    val autoPlaySearchDragons = MyTimer(4 * HOUR_PR_SEC).start()
    val autoPlaySearchDimensionGate = MyTimer((6f * HOUR_PR_SEC).toInt).start()
    val autoPlayDailyBoss = MyTimer((4 * MIN_PR_HOUR + 30) * SEC_PR_MIN).start()
    val autoPlayLegendaryBoss = MyTimer(4 * HOUR_PR_SEC).start()
    val autoPlayPvp = MyTimer(40 * SEC_PR_MIN).start()
    var autoPlayJourneyLastDayCompleted: Long = 0
    var autoPlayBattleLastDayTryUncompleted: Long = 0
    val restartDragonSkyTimer = MyTimer(2 * HOUR_PR_SEC).start()
    val dragonSearchCount = new AtomicRangedInteger(2, 10, 2)

    def incrementNextRushCount() {
        val mod = maxRushCount + 1
        nextRushCount = Some((getNextRushCount() - 1 + mod) % mod)
    }

    def getNextRushCount(): Int = {
        (nextRushCount match {
            case Some(x) => x
            case None => rushCount
        })
    }

    def getNextRushCountAndClear(): Int = {
        val x = getNextRushCount()
        nextRushCount = None
        x + extraRushCount
    }

    def clearNextStageType(): Unit = {
        nextStageOverwritten = None
    }

    def incrementNextStageType(): Unit = {
        nextStageOverwritten =
            Some(nextStageOverwritten match {
                case Some(x) => StageType.next(nextStageType())
                case None => nextStageType() // Force current auto-select as first click
            })
    }

    def setCurrentStageType(stageType: StageType.Value) {
        currentStageType = Some(stageType)
    }

    def nextStageType(): StageType.Value =
        nextStageOverwritten match {
            case Some(x) => x
            case None => selectStage()
        }
}

val jsonFile: Path = pwd / up / "localdata" / "DragonSky.json"
// At every 6th day, we still get the daily reward for clearing all
val daysInBetweenAllTypesClearOffset = 18
val daysInBetweenAllTypesClear = 12 // Last full clear 18/3
val nextFullClearDay = 5 // Last full clear 18/3
val AlexConfig = PlayerConfig(name = Alex,
    // 5 Rushes at max time, requires total 4:27, 4:15 min enough to reach top stage, Rushing 5 times takes 2 minutes
    playStageDuration = (4 * MIN_PR_HOUR + 25) * SEC_PR_MIN, // Rush is around 20 minutes, 4:20 fit right when I have 3 rushes // TODO 10min longer in water, sikkert pga angemoth
    playStageDurationExtra = {
        case StageType.Water => 0
        case _ => 0
    },
    finalStagesDuration = 0 * 60,
    finalStagesDurationExtra = {
        case StageType.Water => 0
        case _ => 0
    },
    rushStagesFixed = 95 + 5, // base,  random, goddess,
    rushStagesRandom = 95,
    rushCount = 5, // No of rushes to attempt
    extraRushCount = 1, // Extra rushes, to ensure we are at max level, this is not included in label
    dailyBossDoMax30 = false,
    selectStage = () => {
        val favorite = StageType.Fire
        val dayOddity = (DateUtils.getDayOfMonth() - daysInBetweenAllTypesClearOffset) % daysInBetweenAllTypesClear
        val hour = DateUtils.getHourOfDay()
//        if (dayOddity != 0) {
        if (DateUtils.getDayOfMonth() != nextFullClearDay) {
            favorite
        } // Going for Brabadon full speed
        else {
            if (hour < 5) {
                StageType.Neutral
            } else if (hour < 10) { // 4 h
                StageType.Water
            } else if (hour < 15) {
                StageType.Wood
            } else if (hour < 20) {
                StageType.Fire // 5 h
            } else {
                favorite
            }
        }
    }
)
val MayaConfig = PlayerConfig(name = Maya,
    // 5 Rushes at max time, requires total 4:27
    playStageDuration = (4 * MIN_PR_HOUR + 20) * SEC_PR_MIN, // Rush is around 20 minutes, 4:20 fit right when I have 3 rushes // TODO 10min longer in water, sikkert pga angemoth
    //        playStageDurationExtra = {
    //                case StageType.Water => 10
    //                case _ => 0
    //            },
    finalStagesDuration = 2 * 60,
    rushStagesFixed = 95 + 5, // 32 base, 7.5 from random, 5 from goddess,
    rushStagesRandom = 95,
    rushCount = 5, // No of rushes to attempt
    extraRushCount = 1,
    selectStage = () => {
        val favorite = StageType.Water
        val dayOddity = (DateUtils.getDayOfMonth() - daysInBetweenAllTypesClearOffset) % daysInBetweenAllTypesClear
        val hour = DateUtils.getHourOfDay()
//        if (dayOddity != 0) {
        if (DateUtils.getDayOfMonth() != nextFullClearDay) {
            favorite
        } else {
            if (hour < 5) {
                StageType.Neutral
            } else if (hour < 10) { // 4 h
                StageType.Fire
            } else if (hour < 15) {
                StageType.Wood // 5 h
            } else if (hour < 20) {
                StageType.Water
            } else {
                favorite
            }
        }
    },
    doLog = true || doLogAllPlayers
)


case class StagePrDuration(stages: Int, minutes: Int) {
    val seconds = minutes * 60
}

// maybe this was Alex
//        val stagesPrHourAtThisTime = hour match {
//            case 0 => 185 // last hour before rush
//            case 1 => 245 // second last hour before rush
//            case 2 => 250
//            case 3 => 272
//            case _ => 300
//        }

// Maya calibration 28.01 02:45
// Alex calibration 29.01 02:00 on Neutral 262,254,233,204, 45 in 14 min, which is 13 stages more in same time, which is just 5 min in difference
//val clearSpeedStagesPrDurationList: List[StagePrDuration] = List(
//    StagePrDuration(306, 60), // AVG 5,1 stage/min
//    StagePrDuration(274, 60), // AVG 4,6 stage/min
//    StagePrDuration(261, 60), // AVG 4,4 stage/min
//    StagePrDuration(63, 18), // AVG 3,5 stage/min
//)
//Alex Water 02.02 7:00 - Water - Angemoth is weak -20% against water, but Elever-Cierabo is strong
//val clearSpeedStagesPrDurationList: List[StagePrDuration] = List(
//    StagePrDuration(286, 61), // AVG 4,7 stage/min
//    StagePrDuration(258, 60), // AVG 4,3 stage/min
//    StagePrDuration(266, 60), // AVG 4,4 stage/min
//    StagePrDuration(60, 14), // AVG 4,3 stage/min
//    StagePrDuration(214, 66), // AVG 3,2 stage/min - max stage 2369
//)
//Alex Fire 02.02 14:00 - Fire is
//val clearSpeedStagesPrDurationList: List[StagePrDuration] = List(
//    StagePrDuration(295, 60), // AVG 4,9 stage/min
//    StagePrDuration(282, 60), // AVG 4,7 stage/min
//    StagePrDuration(249, 60), // AVG 4,2 stage/min
//    StagePrDuration(82, 23), // AVG 3,6 stage/min - max stage 2169 - which is 200 behind
//)
// Alex Fire
//val clearSpeedStagesPrDurationList: List[StagePrDuration] = List(
//    StagePrDuration(295, 60), // AVG 4,9 stage/min
//    StagePrDuration(282, 60), // AVG 4,7 stage/min
//    StagePrDuration(249, 60), // AVG 4,2 stage/min
//    StagePrDuration(214, 66), // AVG 3,2 stage/min - chosen
//    StagePrDuration(68, 9), // AVG 3,2 stage/min - chosen
//)
// Alex Fire 06.02
//val clearSpeedStagesPrDurationList: List[StagePrDuration] = List(
//    StagePrDuration(300, 60), // AVG 5,0 stage/min
//    StagePrDuration(228, 52), // AVG 4,4 stage/min
//    StagePrDuration(148, 38), // AVG 3,9 stage/min
//    StagePrDuration(62, 18), // AVG 3,4 stage/min
//    StagePrDuration(116, 33), // AVG 3,4 stage/min - +4 min
//    StagePrDuration(140, 42), // AVG 3,3 stage/min - +1min
//)
// Alex Fire 26.02 with Adelan-Indermon lvl 1080
val clearSpeedStagesPrDurationList: List[StagePrDuration] = List(
    StagePrDuration(265, 60), // AVG 4.4 stage/min
    StagePrDuration(269, 60), // AVG 4.5 stage/min
    StagePrDuration(228, 61), // AVG 3.7 stage/min
    StagePrDuration(164, 43), // AVG 3.8 stage/min
    StagePrDuration(127, 34), // AVG 3.7 stage/min
)
def computeExpectedStageLevelsByDuration(durationSec: Long): Int = {
    def computeExpectedStageLevelsByDuration(remainingDurationSec: Long, stageDurationIndex: Int, stageDurations: List[StagePrDuration], stages: Int): Int = {
        val stagesPrHourAtThisTime = stageDurations(stageDurationIndex)
        val secondsUsedAtThisStageDuration = Math.min(remainingDurationSec, stagesPrHourAtThisTime.seconds)
        val stagesThisSection = stagesPrHourAtThisTime.stages * secondsUsedAtThisStageDuration / stagesPrHourAtThisTime.seconds
        val stagesTotal = (stages + stagesThisSection).toInt
        if (secondsUsedAtThisStageDuration == remainingDurationSec) {
            stagesTotal
        } else {
            computeExpectedStageLevelsByDuration(remainingDurationSec - secondsUsedAtThisStageDuration, Math.max(0, stageDurationIndex - 1), stageDurations, stagesTotal)
        }
    }

    computeExpectedStageLevelsByDuration(durationSec, clearSpeedStagesPrDurationList.size - 1, clearSpeedStagesPrDurationList, 0)
}

def computeExpectedFinalPlay(player: PlayerConfig): Int = {
    val durationSec: Float = player.finalStagesTimer.remainingOrNextSec()
    (2.0f * durationSec / 60f).toInt // Alex Water 02.02 7:00 stages 2466-2524
} // 80 stages last half hour


val players: List[PlayerConfig] = List(AlexConfig, MayaConfig)
def loadPlayerFile {
    def loadPlayerFileToSeq(): Seq[PlayerConfigJson] = {
        if (exists(jsonFile)) {
            val jsonString = read(jsonFile)
            if (LOG_FILE_DATA) println(s"Raw file: $jsonString")
            try {
                upickle.default.read[Seq[PlayerConfigJson]](jsonString)
            } catch {
                case e: Throwable => println("Failed to read json data file: " + e.getMessage)
                    Nil
            }
        } else {
            Nil
        }
    }

    val playersJson: Seq[PlayerConfigJson] = loadPlayerFileToSeq()
    if (playersJson.nonEmpty) {
        for ((playersJson, p) <- playersJson.zip(players)) {
            PlayerConfigJson.updatePlayerConfig(playersJson, p)
        }
    }
}

def writePlayerFile(): Unit = {
    lock.synchronized {
        // println("Writing file: " + AlexConfig.name + ": " + AlexConfig.rushThenReturnTimer.remainingOrNextMin() + "," + AlexConfig.rushThenReturnTimer.time)
        write.over(jsonFile, upickle.default.write(players.map(p => PlayerConfigJson.create(p))))
    }
}

loadPlayerFile
writePlayerFile()

case class DeviceSerial(serial: String, modelInfo: String, deviceInfo: String) {
    val info = deviceInfo + " " + modelInfo
    private val playerAndName: (PlayerConfig, String, Boolean) = deviceInfo match {
        // Filter the devices I care about
        case "taimen" => (AlexConfig, Alex, true)
        case "beyond1" => (MayaConfig, Maya, true)
        case "OnePlus7Pro" => (AlexConfig, s"$Alex $deviceInfo", true)
        case "OnePlus6" => (MayaConfig, s"$Maya $deviceInfo", true)
        case _ => (AlexConfig.copy(name = info), info, false)
    }

    val player = playerAndName._1
    val name = playerAndName._2
    val isKnownDevice = playerAndName._3
}

import java.awt.{GridBagConstraints, GridBagLayout, Insets}

case class Gui() {

    import java.awt.event._

    import javax.swing._

    case class GuiListener(updateAction: () => Unit)

    val guiListeners = mutable.ListBuffer[GuiListener]()
    val devices: Seq[DeviceSerial] = Adb.devices().filter(_.isKnownDevice || !ENABLE_KNOWN_DEVICE_FILTER).sortBy(_.name)
    val gameHackEngines: Map[DeviceSerial, GameHackEngine] = devices.map(d => d -> GameHackEngine(d)).toMap
    val startPlayingTimers: Map[DeviceSerial, javax.swing.Timer] = devices.map(d =>
        d -> new javax.swing.Timer(TIMER_START_AUTO_PLAYING_MS, _ => autoStartPlaying(d))).toMap
    val scheduledUpdate = new javax.swing.Timer(1000, actionListener => updateAllListeners())
    scheduledUpdate.setRepeats(true)
    scheduledUpdate.start()
    val frame: JFrame = showGui()

    def updateAllListeners() = {
        guiListeners.foreach(_.updateAction())
    }

    def autoStartPlaying(device: DeviceSerial) = {
        println("AutoStart: " + device + " if " + (!isRunning(device)))
        if (!isRunning(device)) gameHackEngines(device).start()
    }

    def restartGui(): Unit = {
        scheduledUpdate.stop()
        startPlayingTimers.values.foreach(_.stop())
        gameHackEngines.values.foreach(_.doQuit())
        Gui()
    }

    def createButtonReset() = createButtonCommonWithRestart("Refresh", () => {})

    //    def createButtonConnectAll() = createButtonCommonWithRestart("Connect", () => Adb.connectPort())

    def createButtonConnectMaya() = createButtonCommonWithRestart("ConnectMayaBS", () => Adb.connectMaya())

    def createButtonConnectAlex() = createButtonCommonWithRestart("Connect Main", () => Adb.connectAlex())

    def createButtonDisconnectAll() = createButtonCommonWithRestart("Disconnect", () => Adb.disconnectAllPorts())

    def createButtonDisconnectMair() = createButtonCommonWithRestart("Disconnect Main", () => Adb.disconnectMainPort())

    def createButtonReturnNow(device: DeviceSerial) = createButtonCommonBaseWithScheduledLabelUpdate(
        () => "ReturnNow: " + gameHackEngines(device).pendingReturnNow.get().toString.substring(0, 1).toUpperCase,
        () => gameHackEngines(device).pendingReturnNow.negate())

    def createButtonSearchDragons(device: DeviceSerial) = createButtonCommonBaseWithScheduledLabelUpdate(
        () => {
            "DragonSearch " + device.player.dragonSearchCount.get()
        },
        () => {
            val oldWasEnabled = gameHackEngines(device).pendingSearchDragons.getAndSet(true)
            if (oldWasEnabled) {
                device.player.dragonSearchCount.increment()
            }
        }
    )

    def createButtonDailyBoss(device: DeviceSerial) = createButtonCommonBase("Daily", () => gameHackEngines(device).pendingDailyBoss.set(true))
    def createButtonDimGate(device: DeviceSerial) = createButtonCommonBase("DimGate", () => gameHackEngines(device).pendingDimensionGate.set(true))

    def createButtonEnableDailyBoss(device: DeviceSerial) = createButtonCommonBaseWithScheduledLabelUpdate(
        () => "Daily: " + gameHackEngines(device).allowDailyBoss.toString.substring(0, 1).toUpperCase,
        gameHackEngines(device).allowDailyBoss.negate)

    def createButtonLegendary(device: DeviceSerial) = createButtonCommonBase("Leg", () => gameHackEngines(device).pendingLegendary.set(true))

    def createButtonPvp(device: DeviceSerial) = createButtonCommonBase("Pvp", () => gameHackEngines(device).pendingPvp.set(true))

    def createButtonPvpAuto(device: DeviceSerial) = createButtonCommonBase("PvpAuto", () => gameHackEngines(device).pendingPvpAuto.set(true))

    def createButtonJourney(device: DeviceSerial) = createButtonCommonBase("Journey", () => gameHackEngines(device).pendingJourney.set(true))

    def createButtonRushCount(device: DeviceSerial) = createButtonCommonBaseWithScheduledLabelUpdate(
        () => {
            val rushExclamation: String = device.player.nextRushCount.map(_ => "!").getOrElse("")
            "Rush " + device.player.getNextRushCount() + rushExclamation
        },
        device.player.incrementNextRushCount
    )

    def createReturnLabel(device: DeviceSerial) = {
        def baseName() = {
            val p = device.player
            val finalStages = computeExpectedFinalPlay(p)
            if (!p.finalStagesTimer.isRunning) {
                if (!p.rushThenReturnTimer.isRunning) {
                    s"No running timers. Rush in ${p.rushThenReturnTimer.nextDurationToString()} Final stages: ${p.finalStagesTimer.nextDurationToString()}"
                } else {
                    val stages = computeExpectedStageLevelsByDuration(p.rushThenReturnTimer.remainingOrNextSec())
                    val rushStages = p.stagesPrRush * p.getNextRushCount()
                    val extraRushStages = p.stagesPrRush * (if (p.getNextRushCount == p.maxRushCount) 0 else p.extraRushCount)
                    val extraRushStagesMsg = if (extraRushStages > 0) s" (+${p.stagesPrRush})" else ""
                    val stagesInclRush = stages + rushStages
                    val remaining = p.rushThenReturnTimer.remainingToString()
                    s"${p.getNextRushCount()}x Rush in $remaining - Stages $stagesInclRush$extraRushStagesMsg + $finalStages" // ($stages + $rushStages )
                }
            } else {
                val remaining = p.finalStagesTimer.remainingToString()
                s"New stage return in $remaining - Stages $finalStages"
            }
        }

        val label = new JLabel(baseName())
        setPreferredSize(label)
        guiListeners += GuiListener(() => label.setText(baseName()))
        label
    }

    private def setPreferredSize(label: JComponent) = {
        label.setPreferredSize(new java.awt.Dimension(200, 100))
    }

    def createButtonReturnTimer(device: DeviceSerial, directionUp: Boolean, isFinalStageButtons: Boolean, incrementMinutes: Int): JButton = {
        val player = device.player
        val prefix = if (directionUp) "+" else "-"
        val sign = if (directionUp) 1 else -1

        def isFinalStages() = player.finalStagesTimer.isRunning

        def baseName() = prefix + incrementMinutes

        val action = () => {
            val maxTimeSec = if (isFinalStageButtons) 90 * 60 else 5 * 60 * 60
            val minTimeSec = 10
            val incrementMin = sign * incrementMinutes
            val timer = if (isFinalStageButtons) player.finalStagesTimer else player.rushThenReturnTimer
            val nextRaw = timer.remainingOrNextSec + incrementMin * 60
            val nextSec = if (nextRaw <= 0 && timer.remainingOrNextSec() > minTimeSec || nextRaw > maxTimeSec) minTimeSec else nextRaw // when going down, go to 1 before negative, when up go to 1 when over max
            if (isFinalStages() && !isFinalStageButtons) {
                player.rushThenReturnTimer.start()
                player.finalStagesTimer.stopAndReset()
                if (directionUp) timer.setNextDuration(5 * 60)
            } else if (nextSec <= 0 && !isFinalStages() && !isFinalStageButtons) {
                player.finalStagesTimer.start()
                player.rushThenReturnTimer.stopAndReset()
            } else {
                timer.setNextDuration(if (nextSec < 0) maxTimeSec else nextSec)
            }
            updateAllListeners()
            writePlayerFile()
        }
        createButtonCommonBaseWithScheduledLabelUpdate(baseName, action)
    }

    def createButtonNextStage(device: DeviceSerial) = {
        def baseName() = "Next " + device.player.nextStageType() + device.player.nextStageOverwritten.map(s => "!").getOrElse("")

        createButtonCommonBaseWithScheduledLabelUpdate(baseName, device.player.incrementNextStageType)
    }

    def createStartStopButton(device: DeviceSerial) = {
        def baseName() = device.name + (if (isRunning(device)) ": Stop" else "")

        createButtonCommonBaseWithScheduledLabelUpdate(baseName, () => {
            if (isRunning(device)) {
                gameHackEngines(device).stop()
                startPlayingTimers(device).restart()
            } else {
                gameHackEngines(device).start()
                startPlayingTimers(device).stop()
            }
        })
    }

    def createButtonCommonWithRestart(name: String, action: () => Unit): JButton = {
        createButtonCommonBase(name, () => {
            gameHackEngines.values.foreach(_.stop())
            frame.dispose()
            action()
            restartGui()
        })
    }

    def createButtonCommonBaseWithScheduledLabelUpdate(name: () => String, action: () => Unit): JButton = {
        val button = createButtonCommonBase(name(), action)
        guiListeners += GuiListener(() => button.setText(name()))
        button
    }

    def createButtonCommonBase(name: String, action: () => Unit): JButton = {
        val button = new JButton(name)
        setPreferredSize(button)
        button.addActionListener((e: ActionEvent) => {
            action()
            updateAllListeners()
            writePlayerFile()
        })
        button
    }

    def isRunning(device: DeviceSerial) = gameHackEngines(device).isStarted()

    //    val frame = new JFrame("Game Hacker")
    //    serials.foreach(s => frame.getContentPane.add(createButton(s)))
    //    frame.pack()
    //    frame.setVisible(true)

    def showGui(): JFrame = { // Create frame with title Registration Demo
        val frame = new JFrame
        frame.setTitle("Game Hacker")
        // Panel to define the layout. We are using GridBagLayout
        val mainPanel = new JPanel
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS))
        val headingPanel = new JPanel
        val panel = new JPanel(new GridBagLayout)
        // Constraints for the layout
        val constr = new GridBagConstraints
        constr.insets = new Insets(5, 5, 5, 5)
        constr.anchor = GridBagConstraints.WEST
        // Set the initial grid values to 0,0
        constr.gridx = 0
        constr.gridy = 0
        // Declare the required Labels
        devices.foreach(device => {
            panel.add(createStartStopButton(device), constr)
            constr.gridx += constr.gridx + 1
            panel.add(createButtonNextStage(device), constr)
            constr.gridx += constr.gridx + 1

            def timerButton(directionUp: Boolean, isFinalStageButtons: Boolean, incrementMinutes: Int): Unit = {
                panel.add(createButtonReturnTimer(device, directionUp, isFinalStageButtons, incrementMinutes), constr)
                constr.gridx += constr.gridx + 1
            }

            timerButton(false, false, 15)
            timerButton(false, false, 3)
            timerButton(true, false, 3)
            timerButton(true, false, 15)
            timerButton(false, true, 2)
            timerButton(true, true, 2)
            autoButton(createButtonRushCount)
            panel.add(createReturnLabel(device), constr)
            constr.gridy += constr.gridy + 1
            constr.gridx = 0

            def autoButton(createButtonFunction: DeviceSerial => JButton): Unit = {
                panel.add(createButtonFunction(device), constr)
                constr.gridx += constr.gridx + 1
            }

            autoButton(createButtonReturnNow)
            autoButton(createButtonSearchDragons)
            autoButton(createButtonDailyBoss)
            autoButton(createButtonDimGate)
            autoButton(createButtonEnableDailyBoss)
            autoButton(createButtonLegendary)
            autoButton(createButtonPvp)
            autoButton(createButtonPvpAuto)
            autoButton(createButtonJourney)
            constr.gridy += constr.gridy + 1
            constr.gridx = 0
        })
        panel.add(createButtonConnectMaya(), constr)
        constr.gridx += constr.gridx + 1
        panel.add(createButtonDisconnectAll(), constr)
        constr.gridx = 0
        constr.gridy += constr.gridy + 1
        panel.add(createButtonConnectAlex(), constr)
        constr.gridx += constr.gridx + 1
        panel.add(createButtonDisconnectMair(), constr)
        constr.gridx = 0
        constr.gridy += constr.gridy + 1
        panel.add(createButtonReset(), constr)
        constr.gridx = 0
        constr.gridy += constr.gridy + 1
        //        constr.gridy = 3
        constr.gridwidth = 2
        constr.anchor = GridBagConstraints.CENTER
        mainPanel.add(headingPanel)
        mainPanel.add(panel)
        // Add panel to frame
        frame.add(mainPanel)
        frame.pack()
        frame.setSize(1200, 400)
        frame.setLocationRelativeTo(null)
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
        frame.setVisible(true)
        frame.addWindowListener(
            new WindowAdapter() {
                override def windowClosing(windowEvent: WindowEvent): Unit = {
                    //                    if (JOptionPane.showConfirmDialog(frame, "Are you sure you want to close this window?", "Close Window?", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) System.exit(0)
                    writePlayerFile()
                }
            }
        )
        frame
    }

}

case class Adb(serial: String) {

    private def adbCommand(args: Seq[String] = Nil): Vector[String] = {
        if (doLogAdb) println(args.mkString(" "))
        val allArgs = Vector("adb", "-s", serial) ++ args
        if (doLogAdbWithSerial) println(allArgs.mkString(" "))
        allArgs
    }

    private def adbShellCommand(args: Seq[String]): Vector[String] = {
        adbCommand(Vector("shell") ++ args)
    }

    def adbExec(args: String*) {
        // Retry necessary on blueStacks
        retry(n = 10, onErrorAction = () => Adb.reconnectPortOnError()) {
            %(adbCommand(args))
        }
    }

    def adbShellExec(args: String*) {
        adbExec(Vector("shell") ++ args: _*)
    }

    //    def execRaw(args: Seq[String]) {
    //        %(adbCommand(args))
    //    }

    //    def execRawForResult(args: Seq[String]): CommandResult = {
    //        %%(adbShellCommand(args))
    //    }
    //    def execRawForResultString(args: Seq[String]): String = {
    //        execRawForResult(args).out.string.trim
    //    }

    def execForResult(args: String*): CommandResult = {
        if (doLogAdb) println(args.mkString(" "))
        // Retry necessary on bluestacks
        retry(5) {
            %%(adbShellCommand(args))
        }
    }

    def execForResultString(args: String*): String = {
        execForResult(args: _*).out.string.trim
    }

    def getBrightness(): String = {
        execForResultString("settings", "get", "system", "screen_brightness")
    }

    // 0-200
    def setBrightness(brightness: Int) {
        adbShellExec("settings", "put", "system", "screen_brightness", brightness + "")
    }

    def setSize(width: Int, height: Int) {
        adbShellExec("wm", "size", width + "x" + height) // may need to swap these dependent on default landscape/portrait
    }

    def getSizeRaw(): CommandResult = {
        execForResult("wm", "size")
    }

    // Can be changed on device settings > Display > DisplaySize.
    def setDensity(density: Int) {
        adbShellExec("wm", "density", density + "")
    }

    def getDensityRaw(): String = {
        execForResultString("wm", "density")
    }

    def getSize(): Vector[Screen] = {
        val phys = "Physical"
        val size = """(Physical|Override) \w*: (\d+)x(\d+)[^\d]*""".r
        val output = getSizeRaw().out.lines
        println(output)
        val screens = for (line <- output) yield {
            line match {
                case size(sizeType, xStr, yStr) =>
                    val x = xStr.toInt
                    val y = yStr.toInt
                    println(s"$x, $y - size: $output")
                    Some(Screen(Math.min(x, y), Math.max(x, y), sizeType == phys))
                case _ => None
            }
        }

        screens.filter(_.isDefined).map(_.get)
    }

    def getSizePhys(): Screen = {
        getSize().filter(_.isPhysicalScreenSize).last
    }

    def getSizeOverridePhys(): Screen = {
        val screens = getSize()
        val phys = screens.find(_.isPhysicalScreenSize)
        val nonPhys = screens.find(!_.isPhysicalScreenSize)
        if (nonPhys.isDefined) {
            nonPhys.get
        }
        else {
            phys.get
        }
    }

    def tap(location: Point): Unit = {
        if (!DRY_RUN) adbShellExec("/system/bin/input", "tap", location.x + "", location.y + "")
    }

    def killDragonSky(): Unit = {
        if (!DRY_RUN) adbShellExec("am", "force-stop", "com.manacore.mod")
    }

    def runDragonSky(): Unit = {
        if (!DRY_RUN) adbShellExec("monkey", "-p", "com.manacore.mod", "1")
    }

    def restartDragonSky(): Unit = {
        killDragonSky()
        runDragonSky()
    }

    def screenshot(folderAndPrefix: String, postfix: String = ""): Unit = {
        if (!DRY_RUN) {
            try {
                val filename = (folderAndPrefix + "_" + DateUtils.getDateAsString(new Date()) + s"${postfix}.png")
                    .replaceAll("""\s|\(|\)""", "_")
                val file = s"/sdcard/$filename"
                println(s"Screenshot to $file")
                adbShellExec("screencap", "-p", file)
                val dir: Path = dirScreenShots / folderAndPrefix
                adbExec("pull", file, dir.toString())
                adbShellExec("rm", file)
            } catch {
                case e: Throwable =>
                    println("Error capturing screenshot")
                    e.printStackTrace()
            }
        }
    }

    def swipe(from: Point, to: Point, durationMs: Int): Unit = {
        if (!DRY_RUN) adbShellExec("/system/bin/input", "swipe", from.x + "", from.y + "", to.x + "", to.y + "", durationMs + "")
    }
}

object Adb {
    def reconnectPortOnError() {
        // A different option is to just reconnect the serial in question and only if its localhost something
        println("Connecting Maya+Alex")
        connectAlex() // Necessary once, where it used localhost:5555 and it suddenly no longer worked, but reconnecting worked
        connectMaya()
    }

    def connectAllPort() {
        connectAlex()
        connectMaya()
    }

    def connectAlex() {
        %("adb", "connect", "localhost:5555")
    }

    def connectMaya() {
        %("adb", "connect", "localhost:5565") // Bluestacks Maya on Lenovo
        %("adb", "connect", "localhost:5655") // Bluestacks Maya on Muaddib
    }

    def disconnectAllPorts() {
        %("adb", "disconnect", "localhost:5555")
        %("adb", "disconnect", "localhost:5565") // lenovo maya
        %("adb", "disconnect", "localhost:5655") // muaddib maya
    }

    def disconnectMainPort() {
        %("adb", "disconnect", "localhost:5555")
    }

    def devices(): Seq[DeviceSerial] = {
        def prune(s: String) = s.replaceAll("_", " ").replaceAll("  ", " ")

        retry(10) {
            val lines = %%("adb", "devices", "-l").out.lines
            println(s"devices: " + lines.mkString("\n"))
            if (lines.size == 0) throw new IllegalArgumentException("Zero lines, retry")
            val regSerial = """([^\s]+)\s+device .*model:([^ ]*).*device:([^ ]*).*""".r
            val devs: Seq[DeviceSerial] = lines.flatMap(line => {
                line match {
                    case regSerial(serial, modelInfo, deviceInfo) => Seq(DeviceSerial(serial, prune(modelInfo), prune(deviceInfo)))
                    case _ => Nil
                }
            })
            devs
        }
    }
}

case class Point(x: Int, y: Int, name: String = "")

object Point {
    def random(start: Point, end: Point): Point = Point(r.nextInt(end.x - start.x) + start.x, r.nextInt(end.y - start.y) + start.y)
}

case class Range(begin: Int, end: Int) {
    def delta = end - begin

    def center = (end + begin) / 2

    def random(): Int = r.nextInt(delta) + begin

    def iterate(steps: Int): IndexedSeq[Int] = {
        if (steps < 2) throw new IllegalArgumentException("Min two steps")
        val stepSize = delta / (steps - 1)
        // I think dragons migrate to the bottom if we always start dragging from the top, they drag dragons from far range and most are in the center
        (0 until steps).map(idx => idx * stepSize + begin)
    }

    @tailrec
    final def randomWithBlacklist(blacklist: Range): Int = {
        val r = random()
        val inBlacklist = blacklist.contains(r)
        //if (inBlacklist) println(r + " contained in blacklist range " + blacklist) else println(r + " OK blacklist " + blacklist)
        if (!inBlacklist) r else randomWithBlacklist(blacklist)
    }

    def contains(other: Int): Boolean = begin <= other && other <= end

}

case class Rectangle(begin: Point, end: Point) {
    val xRange = Range(begin.x, end.x)
    val yRange = Range(begin.y, end.y)

    def random(): Point = Point(xRange.random, yRange.random)

    // Blacklists in both x and y ranges, so only corners are allowed when blacklisting center of current rectangle
    def randomWithBlacklist(blacklist: Rectangle): Point = {
        Point(xRange.randomWithBlacklist(blacklist.xRange), yRange.randomWithBlacklist(blacklist.yRange))
    }

    def randomWithBlacklistY(blacklist: Rectangle): Point = {
        Point(xRange.random(), yRange.randomWithBlacklist(blacklist.yRange))
    }

    def center(): Point = Point(xRange.center, yRange.center)

    def contains(point: Point): Boolean = xRange.contains(point.x) && yRange.contains(point.y)

    def containsEitherCoordinate(point: Point): Boolean = xRange.contains(point.x) || yRange.contains(point.y)
}

case class Screen(width: Int, height: Int, isPhysicalScreenSize: Boolean) {
    def fromPct(pct: Float, name: String): Point = Point((width * pct).toInt, (height * pct).toInt, name)

    def fromPct(xPct: Float, yPct: Float, name: String = ""): Point = Point((width * xPct).toInt, (height * yPct).toInt, name)

    def fromPctD(xPct: Double, yPct: Double, name: String = ""): Point = Point((width * xPct).toInt, (height * yPct).toInt, name)
}

case class GameHackEngine(device: DeviceSerial, changesize: Boolean = false) {
    val quit = new AtomicBoolean()
    val isRunningInLoop = new AtomicBoolean()
    val player = device.player
    val doLog = doLogActions && player.doLog

    private class RunLock {
        private var doRun: Boolean = false
        //        private var isRunningInLoopInternal: Boolean = false

        def doWait(): Unit = {
            this.synchronized {
                this.wait()
            }
        }

        def isRunning() = this.synchronized {
            this.doRun
        }

        def setRun(doStart: Boolean) {
            this.synchronized {
                this.doRun = doStart
                this.notify()
            }
        }

        //
        //        def isRunningInLoop() = this.synchronized {
        //            this.isRunningInLoopInternal
        //        }
        //
        //        def setRunningInLoop(isRunningInLoop: Boolean) {
        //            this.synchronized {
        //                this.isRunningInLoopInternal = !isRunningInLoop
        //                this.notify()
        //            }
        //        }
    }

    class AtomicNegatableBoolean(initial: Boolean = false) extends AtomicBoolean {
        set(initial)

        def negate() {
            val bool = get()
            println("old" + bool)
            set(!bool)
            println("new " + get())
        }
    }

    class AtomicMaxedInteger(val min: Int, val max: Int, val incrementSize: Int = 1) {
        if (min > max) throw new IllegalArgumentException()
        if (increment > max - min) throw new IllegalArgumentException()
        var value: Int = min

        def get(): Int =
            this.synchronized {
                value
            }

        def increment(): Int = {
            this.synchronized {
                value = value + 1
                if (value > max) value = min
                value
            }
        }
    }

    private val lock = new RunLock()
    val adb = Adb(device.serial)
    val allowDailyBoss = new AtomicNegatableBoolean(true)
    val pendingReturnNow = new AtomicNegatableBoolean()
    val pendingSearchDragons = new AtomicBoolean()
    val pendingDimensionGate = new AtomicBoolean()
    val pendingDailyBoss = new AtomicBoolean()
    val pendingLegendary = new AtomicBoolean()
    val pendingPvp = new AtomicBoolean()
    val pendingPvpAuto = new AtomicBoolean()
    val pendingJourney = new AtomicBoolean()
    //    def isRunningInLoop() = this.synchronized {
    //        lock.isRunningInLoop()
    //    }

    def isStarted() = this.synchronized {
        lock.isRunning()
    }

    def start() {
        lock.setRun(true)
    }

    def stop() {
        lock.setRun(false)
        thread.interrupt()
    }

    def doQuit() {
        stop()
        quit.set(true)
        thread.interrupt()
    }

    def init(): Unit = {
        println(device)
        if (changesize) {
            val originalScreen = adb.getSizePhys()
            val newWidth = 720
            val newHeight = originalScreen.height * newWidth / originalScreen.width
            println(s"Original Screen size detected: " + originalScreen)
            println(adb.execForResultString("wm", "size"))
            println(adb.execForResultString("wm", "density"))
            // OnePlus 7 pro density default 560 when 1440p.
            // When 1080p it is 480 density, corresponding to 640 at 1440p
            adb.setSize(newWidth, newHeight)
            adb.setDensity(newWidth * 560 / 1440) // 280 for 720p
            println(adb.getDensityRaw())
        }
        val originalBrightness = adb.getBrightness()
        println(originalBrightness)
        adb.setBrightness(10)
    }

    init()

    val screen: Screen = adb.getSizeOverridePhys()
    println(s"Screen size detected: $screen")
    val pointGoddessClose = screen.fromPct(0.5f, 0.93f, "GoddessClose") // When clicking chest in legendary screen, goddess is shown for player
    val pointChest = screen.fromPct(1300f / 1440, 2600f / 3120, "Chest")
    // this also closes Legendary boss fight when done
    val pointHatchWithPvpDeactivateEgg = screen.fromPct(0.90f, 0.98f, "HatchEgg") // Clicks Pvp Deactivate 0.93f, 0.95f, text also clickable
    val pointHatchEgg = screen.fromPct(0.97f, 0.99f, "HatchEgg") // Clicks Pvp Deactivate 0.93f, 0.95f
    //    val pointCloseCommonPvpAutoDisable = screen.fromPct(.93f, 0.97f, "CloseCommon") // bottom right
    val pointCloseCommonNoPvpAutoDisable = screen.fromPct(.93f, 0.99f, "CloseCommon") // bottom right
    val pointCloseDragonGui = pointCloseCommonNoPvpAutoDisable
    val pointReconnect = screen.fromPct(0.4f, 0.62f, "Reconnect")
    val pointNoToShinyFusingAndPvpQuitBattleDialog = screen.fromPct(0.2f, 0.71f, "NoToShinyFusing") // Not reconnect
    val pointMenu = screen.fromPct(.08f, 0.95f, "Menu")
    val pointMenuReturn = screen.fromPct(.08f, 0.47f, "MenuReturn")
    val pointFailedStageReturn = screen.fromPct(.50f, 0.25f, "FailedStageReturn")
    val pointReturnQ1Yes = screen.fromPct(.70f, 0.85f, "ReturnQ1Yes")
    val pointReturnQ2Yes = screen.fromPct(.40f, 0.70f, "ReturnQ2Yes")
    val pointMainChooseType = screen.fromPct(.60f, 0.50f, "MainChooseType")
    val pointMainChooseTypeNext = screen.fromPct(.60f, 0.30f, "MainChooseTypeNext")
    val pointMainChooseTypePrevious = screen.fromPct(.60f, 0.7f, "MainChooseTypePrevious")
    val pointSwipeCenter = screen.fromPct(.60f, 0.55f, "SwipeCenter")
    val pointSwipeUpScrollDown = screen.fromPct(.60f, 0.3f, "SwipeUpScrollDown")
    val pointSwipeDownScrollUp = screen.fromPct(.60f, 0.8f, "SwipeDownScrollUp")
    val pointMainChooseTypeConfirm = screen.fromPct(.70f, 0.70f, "MainChooseTypeConfirm")
    val pointRush = screen.fromPct(.906f, 0.46f, "Rush")

    val pointMystic = screen.fromPct(0.5f, 0.5f, "Mystic") // X=33-67%, Y=46-53%

    val pointMenuPvp = screen.fromPct(.08f, 0.66f, "MenuPvp")
    val pointPvPAutoOn = screen.fromPct(.50f, 0.56f, "PvPAutoOn")
    val pointPvPAutoOff = screen.fromPct(.69f, 0.56f, "PvPAutoOff")
    val pointPvPStart = screen.fromPct(.50f, 0.50f, "PvPStart")
    val pointPvPStartWithTeam = screen.fromPct(.50f, 0.75f, "PvPStartWithTeam")
    val pointPvPCloseIfNoFight = screen.fromPct(.50f, 0.87f, "PvPCloseIfNoFight")
    val pointPvPCloseAutoHelp = screen.fromPct(.45f, 0.71f, "PvPCloseAutoHelp") // y inside shiny yes/no
    val pointPvPCloseGoddess = screen.fromPct(.35f, 0.62f, "PvPCloseGoddess")
    val pointPvPClose = pointCloseCommonNoPvpAutoDisable

    val pointMenuExplore = screen.fromPct(.08f, 0.76f, "MenuExplore")
    val pointMenuExploreSearch = screen.fromPct(.28f, 0.75f, "MenuExploreSearch")
    val pointMenuExploreDimGate = screen.fromPct(.29f, 0.55f, "MenuExploreDimensionGate")
    val pointSearchDragonTab = screen.fromPct(.22f, 0.95f, "SearchDragonTab")
    val pointSearchEssenceTab = screen.fromPct(.64f, 0.95f, "SearchEssenceTab")
    val pointSearchDragonTame1 = screen.fromPct(.70f, 0.46f, "SearchDragonTame1")
    val pointSearchDragonTame2 = screen.fromPct(.70f, 0.86f, "SearchDragonTame2")
    val pointSearchDragonTameAgain = screen.fromPct(.50f, 0.69f, "SearchDragonTameAgain")
    val pointSearchDragonTameClose = screen.fromPct(.50f, 0.79f, "SearchDragonTameClose")
    val pointSearchNext = screen.fromPct(.86f, 0.10f, "SearchNext")
    // The two cancels do not overlap
    val pointSearchNextWarningNoMoreSearchAndAd_SelectCancel = screen.fromPct(.18f, 0.66f, "SearchNextButNoMore=>Cancel")
    val pointSearchNextWarningCancel = screen.fromPct(.28f, 0.60f, "SearchNextWarning=>Cancel (Essence+Tame)") // take bottom y so less likely to overlap buy 44-48%
    val pointSearchNextWarningOk = screen.fromPct(.71f, 0.58f, "SearchNextWarningOK")
    val pointSearchEssenceFree = screen.fromPct(.26f, .29f, "SearchEssenceFree")
    val pointSearchEssenceGold = screen.fromPct(.73f, .29f, "SearchEssenceGold")
    val pointSearchEssenceCollectPurchase = screen.fromPct(.76f, .62f, "SearchEssence Collect/Purchase")
    val pointSearchEssenceClosePopup = screen.fromPct(.50f, .70f, "SearchEssence Close popup")

    val pointSearchDimensionGateInitialRift = screen.fromPct(.33f, .34f, "InitialRift")
    val pointSearchDimensionGateEvilGateAfterChoosingRift = screen.fromPct(.68f, .21f, "FollowingEvil")
    val pointSearchDimensionGateChooseEnter1X = screen.fromPct(.20f, .70f, "DimGate 1x")
    val pointSearchDimensionGateChooseEnter5X = screen.fromPct(.50f, .70f, "DimGate 5x")
    val pointSearchDimensionGateChooseEnter10X = screen.fromPct(.80f, .70f, "DimGate 10x")
    val pointSearchDimensionGateChooseEnterDimension = screen.fromPct(.50f, .84f, "DimGate Enter")
    val pointSearchDimensionGateChooseChallengeAfterEnter = screen.fromPct(.50f, .59f, "DimGate ChallengeAfterEnter")
    val pointSearchDimensionGateChooseChallengeClose = screen.fromPct(.50f, .71f, "DimGate ChallengeAfterEnter")
    val pointSearchDimensionGateSkipEffects = screen.fromPct(.25f, .80f, "DimGate ChallengeAfterEnter") // Left side, so it might not collide with start

    val pointSearchClose = screen.fromPct(.92f, 0.97f, "SearchClose")
    val rectangleDragonGiftSpawn = Rectangle(screen.fromPct(.26f, 0.53f), screen.fromPct(.74f, 0.63f))

    val pointMenuBattle = screen.fromPct(.22f, 0.62f, "MenuBattle")
    val pointMenuBattleJourney = screen.fromPct(.40f, 0.35f, "MenuBattleJourney")
    val pointMenuBattleLegendaryBoss = screen.fromPct(.40f, 0.47f, "MenuBattleLegendaryBoss")
    val pointMenuBattleDailyBoss = screen.fromPct(.40f, 0.59f, "MenuBattleDailyBoss")
    val pointLegendaryStart = screen.fromPct(.40f, 0.53f, "LegendaryStart")
    val pointLegendaryStartTeamChosen = screen.fromPct(.50f, 0.60f, "LegendaryStartTeamChosen")
    val pointLegendaryRewardAndTeamClose = screen.fromPct(.50f, 0.69f, "LegendaryRewardAndTeamClose")
    val pointLegendaryClose = pointCloseCommonNoPvpAutoDisable

    val pointDailyBossNeutral = screen.fromPctD(.26, 0.36, "DailyBossNeutral")
    val pointDailyBossFire = screen.fromPctD(.74, .36, "DailyBossFire")
    val pointDailyBossWater = screen.fromPctD(.26, .67, "DailyBossWater")
    val pointDailyBossWood = screen.fromPctD(.74f, .67, "DailyBossWood")
    val pointDailyBossFinishNow_WhenUnmaxed = screen.fromPct(.17f, .74f, "DailyBossFinishNow")
    val pointDailyBossEnterHighestCompleted_WhenUnmaxed = screen.fromPct(.46f, .71f, "DailyBossEnterCurrentLevel")
    val pointDailyBossEnterHighestCompleted_WhenMaxed = pointDailyBossEnterHighestCompleted_WhenUnmaxed // TODO
    // When maxed to level 25 (in old game gui) we don't know which of the last two is the uncompleted
    val pointDailyBossEnterNextUncompletedChallenge_WhenUnmaxed = screen.fromPctD(.77, .71, "DailyBossEnterUncompleted")
    // Not updated _WhenMaxed to new gui, but coordinates probably also work, otherwise they'll work if moving a bit to the right
    val pointDailyBossFinishThirdHighest_WhenMaxed = pointDailyBossFinishNow_WhenUnmaxed
    val pointDailyBossEnterSecondHighest_WhenMaxed = pointDailyBossEnterHighestCompleted_WhenUnmaxed
    val pointDailyBossFinishSecondHighest_WhenMaxed = pointDailyBossFinishNow_WhenUnmaxed
    val pointDailyBossEnterHighest_WhenMaxed = pointDailyBossEnterHighestCompleted_WhenUnmaxed
    val pointDailyBossFinishHighest_WhenMaxed = pointDailyBossFinishNow_WhenUnmaxed
    val pointDailyBossCloseReward = screen.fromPct(.50f, 0.10f, "DailyBossCloseReward")
    val pointDailyBossClose = pointCloseCommonNoPvpAutoDisable

    val pointJourneyGold = screen.fromPct(.18f, .45f, "JourneyGold")
    val pointJourneySoulStone = screen.fromPct(.50f, .45f, "JourneySoulStone")
    val pointJourneyLifeStone = screen.fromPct(.82f, .45f, "JourneyLifeStone")
    val pointJourneyRewardOpen = screen.fromPct(.50f, .73f, "JourneyRewardOpen")
    val pointJourneyFight = screen.fromPct(.50f, .57f, "JourneyFight")
    val pointJourneyFightClose = screen.fromPct(.50f, .76f, "JourneyFightClose")
    val pointJourneyClose = pointCloseCommonNoPvpAutoDisable

    // Expanded Click radius in DragonSky game is around 0.5% Text in PvP Deactivate button is part of button click radius
    // PVP button top left 79,7% x 94%, bottom right=96%,97.1% (PVP deactivate)
    // Leg button top left 86.5% x 97%, bottom right=94.3% x 100%
    // Max y: opened shop once with max y=.93 on 720p screen
    val xOffsetPct = .25f
    val yOffsetPctStart = .81f
    val yOffsetPctEnd = 0.90f

    val xOffsetInnerPct = .30f
    val yOffsetInnerPctStart = .81f
    val yOffsetInnerPctEnd = 0.93f // Dragons are spawned below 0.88, .93 is a bit above bottom spawn location, but seems to pick up dragons

    val yOffsetRandomPctStart = .83f
    val yOffsetRandomPctEnd = 0.91f
    // dragons spawn at .3, but drag radius is out to .35, but at .35 a dragon never got picked up, so give some slack for random drags to include that area. Tested to
    // work well with .3
    val xOffsetRandomPct = .30f

    // Random to should probably be quadratic for optimal random merging. Height is 2.1x Width, so
    val yOffsetRandomToPctStart = .85f
    val yOffsetRandomToPctEnd = 0.89f
    val xOffsetRandomToPct = .45f
    val xOffsetRandomBlacklistPct = .32f

    val pointOuterStart = screen.fromPct(xOffsetPct, yOffsetPctStart, "OuterStart")
    val pointOuterEnd = screen.fromPct(1 - xOffsetPct, yOffsetPctEnd, "OuterEnd")
    val pointInnerStart = screen.fromPct(xOffsetInnerPct, yOffsetInnerPctStart, "InnerStart")
    val pointInnerEnd = screen.fromPct(1 - xOffsetInnerPct, yOffsetInnerPctEnd, "InnerEnd")
    val rectOuter = Rectangle(pointOuterStart, pointOuterEnd)
    val rectInner = Rectangle(pointInnerStart, pointInnerEnd)
    val rectRandomFrom = Rectangle(screen.fromPct(xOffsetRandomPct, yOffsetRandomPctStart), screen.fromPct(1 - xOffsetRandomPct, yOffsetRandomPctEnd))
    val rectRandomTo = Rectangle(screen.fromPct(xOffsetRandomToPct, yOffsetRandomToPctStart), screen.fromPct(1 - xOffsetRandomToPct, yOffsetRandomToPctEnd))
    val rectRandomFromBlacklistCenter = Rectangle(screen.fromPct(xOffsetRandomBlacklistPct, yOffsetRandomToPctStart), screen.fromPct(1 - xOffsetRandomBlacklistPct, yOffsetRandomToPctEnd))
    val dragTo = Point(screen.width / 2, ((pointOuterStart.y + pointOuterEnd.y) / 2).toInt)
    val pointLoginClosePopUpWithProgressAfterReconnect = screen.fromPct(0.5f, 0.93f, "StartClosePopUpWithProgressAfterReconnect")

    println(s"xStart-xEnd: ${pointOuterStart.x}-${pointOuterEnd.x}")
    println(s"yStart-yEnd: ${pointOuterStart.y}-${pointOuterEnd.y}")
    println(s"Outer Random Rect: ${rectRandomFrom.begin}-${rectRandomFrom.end}")

    var pName = device.player.name

    def time = AlexDateUtils.DateUtils.formatDate(new Date(), AlexDateUtils.DateUtils.TIME_FORMAT)

    def logPrefix = s"$pName $time Thread ${Thread.currentThread().getId}:"

    var theAdb: Adb = _

    // Performance test of adb shell. 10x adb shell tap took 3.9s, one adb shell with 10x tap took 3.4s
    // With swipe it was 6.3s vs 6.6s
    // measure-command {foreach ($i in 1..10) { adb shell /system/bin/input tap 975 2100 }}
    def tap(location: Point, logInfo: String = null, sleepDuration: Int = 0, repeat: Int = 1): Long = {
        val logInfoMsg = if (logInfo != null) s"$logInfo " else ""
        // TODO Maya sometimes stops here, with sleep=0. With above as lst log statement. Perhaps the ADB is stuck
        // there were two adb processes. Try killing them next time.
        //    println(s"adb shell /system/bin/input tap ${location.x} ${location.y}")
        val timer = SimpleTimer()
        loop(repeat) {
            if (lock.isRunning()) {
                if (doLog) println(s"$logPrefix $logInfoMsg${location.name} tap $location Sleep=$sleepDuration")
                adb.tap(location) // Sometimes ADB process hangs
                // Sleep after tapping, auto-play relies on this feature
                if (sleepDuration > 0) sleep(sleepDuration)
            }
        }
        timer.timePassed()
    }

    def hatch(): Unit = {
        tap(pointHatchEgg, "hatching")
    }

    // Mystic/Legendary button appear after 3.5-4s merge animation and lasts 4 sec
    def mysticUpgradeDragonMergeToLegendaryOrMystic(): Unit = {
        if (doMysticTapAfterMergeDragons) {
            tap(pointMystic)
        }
    }

    // Chest appears 1:15 after last chest was collected, and not after chest last appeared. So we collect frequently to get as many as possible.
    // There's around 1.5 sec between each rateLimit is called (at the moment), 2 drags take 1 sec
    def collectChest(): Unit = {
        tap(pointCloseDragonGui, "CloseDragonGui")
        tap(pointChest, "Chest")
        tap(pointGoddessClose, "GoddessClose")
    }

    case class RateLimiter(rate: Int, info: String) {
        var rateLimitedCounter = 0
        var timer = MyTimer().start()

        def apply(exec: () => Unit): Unit = {
            rateLimitedCounter += 1
            if (rateLimitedCounter % rate == 0) {
                exec.apply()
                // println(s"$info: Fired with $rate: duration ${timer.delta()} sec - ${timer.deltaDuration()} ")
                rateLimitedCounter = 0
                timer.start()
            } else {
                //                println(s"$rate: $rateLimitedCounter - duration ${timer.deltaSecs()} sec")
            }
        }
    }

    case class TimeLimiter(durationSec: Long, info: String) {
        var timer = MyTimer(durationSec).start()

        def apply(exec: () => Unit): Unit = {
            if (timer.isTimePassed()) {
                exec.apply()
                println(s"$logPrefix $info: time since last ${timer.delta()} sec")
                timer.restart()
            } else {
                println(s"$logPrefix $info: time since last ${timer.delta()} sec - Too recent not executed")
            }
        }
    }

    // 3 is fine, but it seems the number of daily chests is limited, no more chests after running all night, 50 perhaps to rare with 75%
    // nightly, but there were still chests in the morning
    val mysticTimeLimiter = TimeLimiter(2, "Mystic")
    val noToShinyTimeLimiter = TimeLimiter(5, "NoToShiny")
    val chestRateLimiter = RateLimiter(30, "Chest")
    val eggHatchRateLimiter = RateLimiter(2, "Hatch") // 1 is too frequent, it cannot merge dragons quickly enough at 75% CPU Power

    //    See also adb shell  /system/bin/input --help
    // DO NOT use for dragon merges, as mystic button does not get tapped
    def dragOnly(from: Point, to: Point, swipeDurationMs: Int = 250, logInfo: String = null): Unit = {
        val logInfoMsg = if (logInfo != null) s" $logInfo" else ""
        if (lock.isRunning()) {
            if (doLog) println(s"$logPrefix$logInfoMsg: $from -> $to")
            adb.swipe(from, to, swipeDurationMs)
            if (doThrottleSleep) {
                sleep(throttleSleep)
            }
        }
    }

    def dragForDragonMergeWithHatchAndMysticTaps(from: Point, to: Point, durationMs: Int = 250, logInfo: String = null): Unit = {
        if (lock.isRunning()) {
            eggHatchRateLimiter(() => hatch())
            chestRateLimiter(() => collectChest())
            dragOnly(from, to, durationMs, logInfo)
            if (doMysticTapAfterMergeDragons) mysticTimeLimiter(() => mysticUpgradeDragonMergeToLegendaryOrMystic())
            noToShinyTimeLimiter(() => tap(pointNoToShinyFusingAndPvpQuitBattleDialog))
        }
    }

    def dragDrawCorners(info: String, rect: Rectangle): Unit = {
        dragDrawCorners(info, rect.begin, rect.end)
    }

    def dragDrawCorners(info: String, from: Point, to: Point): Unit = {
        if (LOG_DRAW_CORNERS) {
            val logInfo = s"Drawing $info"
            // Effectively draw square if debug draw pointer is enabled
            val diagonalOne = Point(from.x, to.y) // lower left (from being upper left)
            val diagonalTwo = Point(to.x, from.y) // upper right
            dragOnly(from, diagonalOne, 0, logInfo)
            dragOnly(diagonalOne, to, 0, logInfo)
            dragOnly(to, diagonalTwo, 0, logInfo)
            dragOnly(diagonalTwo, from, 0, logInfo)
        }
    }

    def doGridFromPointsToFunc(area: Rectangle, stepsX: Int, stepsY: Int, innerFunc: Point => Unit): Unit = {
        for (x <- area.xRange.iterate(stepsX);
             y <- area.yRange.iterate(stepsX)) {
            val p = Point(x, y)
            println(s"$area: => $p")
            innerFunc(p)
        }
    }

    def dragInToCenter(xy: Point) {
        if (xy != dragTo) {
            dragForDragonMergeWithHatchAndMysticTaps(xy, dragTo)
        }
    }

    def dragInToRandom(xy: Point) {
        dragForDragonMergeWithHatchAndMysticTaps(xy, rectRandomTo.random())
    }

    def dragOutInOut(xy: Point) {
        if (xy != dragTo) {
            dragForDragonMergeWithHatchAndMysticTaps(dragTo, xy)
            // Important to drag away again and back to center, to merge it
            dragForDragonMergeWithHatchAndMysticTaps(xy, dragTo)
            dragForDragonMergeWithHatchAndMysticTaps(dragTo, xy)
        }
    }

    object ReturnType extends Enumeration {
        val Menu, FailedStage = Value
    }

    def sleep(durationMs: Long, logInfo: String = null): Unit = {
        if (!DRY_RUN && lock.isRunning() && durationMs > 0) {
            try {
                if (doLog && logInfo != null) println(s"$logPrefix Sleep=$durationMs")
                Thread.sleep(durationMs)
            } catch {
                case _ => println("Sleep interrupted")
            }
        }
    }

    def doPvpBattle(auto: Boolean): Unit = doPvpBattle(auto, false)

    def doPvpBattleAutoDisable(): Unit = doPvpBattle(false, true)

    def doPvpBattle(auto: Boolean, autoDisable: Boolean): Unit = {
        if (doLog) println(s"$pName PvPBattle auto=" + auto)
        // TODO Fix close auto-close in case fight is longer than expected
        if (autoDisable) {
            // in case a pvp is currently playing
            tap(location = pointHatchWithPvpDeactivateEgg, sleepDuration = sleepDurationMenu)
            // I think this also closes 'Quit pvp battle' dialog
            tap(pointNoToShinyFusingAndPvpQuitBattleDialog)
        }

        val pvpFightDuration = 60 * MILLI_PR_SEC
        tap(location = pointMenu, sleepDuration = sleepDurationMenu)
        tap(location = pointMenuPvp, sleepDuration = sleepDurationMenu)
        dragOnly(pointSwipeCenter, pointSwipeDownScrollUp, 250)
        sleep(500) // wait for scroll to top
        dragOnly(pointSwipeCenter, pointSwipeDownScrollUp, 250)
        sleep(500) // wait for scroll to top
        if (autoDisable) {
            player.pvpAutoActive = false
            writePlayerFile()
            tap(location = pointPvPAutoOff, sleepDuration = sleepDurationMenu)
        } else if (auto) {
            player.pvpAutoActive = true
            tap(location = pointPvPAutoOn, sleepDuration = sleepDurationMenu)
            writePlayerFile()
        } else {
            // Auto fucks with return and stuff
            tap(location = pointPvPAutoOff, sleepDuration = sleepDurationMenu)
            //            tap(location = pointPvPAutoOn, sleepDuration = sleepDurationMenu)
            tap(location = pointPvPStart, sleepDuration = sleepDurationMenu)
            tap(location = pointPvPStartWithTeam, sleepDuration = sleepDurationMenu)
            // This will open again if done after fight making us stuck, so above start fight should be a short sleep
            tap(location = pointPvPCloseIfNoFight, sleepDuration = sleepDurationMenu)
            sleep(pvpFightDuration)
        }
        doPvpClose()
    }

    def doPvpClose(): Unit = {
        // safe close pvp works everywhere
        tap(location = pointPvPCloseAutoHelp, sleepDuration = sleepDurationMenu) // Should be safe if not needed
        tap(location = pointPvPClose, sleepDuration = sleepDurationMenu)
        tap(location = pointPvPCloseGoddess, sleepDuration = sleepDurationMenu)
        tap(location = pointPvPClose, sleepDuration = sleepDurationMenu)
    }

    def doBattleLegendaryBoss() = {
        val legendaryFightDuration = 100 * MILLI_PR_SEC // Best to pause even if there's is no fight, so I know I don't merge or try to return when there's a fight going on
        tap(location = pointMenu, sleepDuration = sleepDurationMenu)
        tap(location = pointMenuBattle, sleepDuration = sleepDurationMenu)
        tap(location = pointMenuBattleLegendaryBoss, sleepDuration = sleepDurationMenu) // Don't repeat this

        repeat(2) {
            dragOnly(pointSwipeCenter, pointSwipeDownScrollUp, 250)
            sleep(500) // wait for scroll to top
        }

        tap(location = pointLegendaryStart, sleepDuration = sleepDurationMenu, repeat = 3)
        tap(location = pointLegendaryStartTeamChosen, sleepDuration = sleepDurationMenu, repeat = 2)
        tap(location = pointLegendaryStartTeamChosen, sleepDuration = legendaryFightDuration)
        // Not really necessary with team close, as close will work regardless of team challenge dialog open
        tap(location = pointLegendaryRewardAndTeamClose, sleepDuration = sleepDurationMenu, repeat = 3) // Reward
        tap(location = pointLegendaryRewardAndTeamClose, sleepDuration = sleepDurationMenu, repeat = 3) // Reward
        tap(location = pointLegendaryClose, sleepDuration = sleepDurationMenu, repeat = 3) // closes regardless of other player popup
    }

    def doBattleDailyBoss(tryUncompleted: Boolean) = {
        if (allowDailyBoss.get()) {
            println(s"$pName doBattleDailyBoss: tryUncompleted=" + tryUncompleted)
            // Fight takes between 30-180s
            val dailyBossFightDuration = 90 * MILLI_PR_SEC // Best to pause even if there's is no fight, so I know I don't merge or try to return when there's a fight going on
            tap(location = pointMenu, "Menu", sleepDuration = sleepDurationMenu)
            tap(location = pointMenuBattle, "MenuBattle", sleepDuration = sleepDurationMenu)
            tap(location = pointMenuBattleDailyBoss, "MenuBattleDailyBoss", sleepDuration = sleepDurationMenu)

            // This can probably be just once now
            def selectType() {
                tap(location = pointDailyBossFire, "DailyBossFire", sleepDuration = 100) // prefer fire, but alex is below on level on fire
                tap(location = pointDailyBossWood, "DailyBossWood", sleepDuration = 100)
                tap(location = pointDailyBossNeutral, "DailyBossNeutral", sleepDuration = 100)
                tap(location = pointDailyBossWater, "DailyBossWater", sleepDuration = 100)
                sleep(1500) // Wait for it to scroll there
            }

            // Nothing happens if trying to enter a unaxed daily boss, when boss is maxed and view is scrolled to the bottom. So we try both
            def fight(pUnmaxed: Point, pMaxed: Point = null, info: String, fight: Boolean) {
                val sleepFight = if (fight) dailyBossFightDuration else sleepDurationMenu
                val sleepAfterFight = if (fight) 5 * MILLI_PR_SEC else 500
                selectType() // not necessary after finish now
                tap(location = pUnmaxed, s"DailyBoss$info", sleepDuration = 50)
                if (pMaxed != null) tap(location = pMaxed, s"DailyBoss$info", sleepDuration = sleepFight)
                tap(location = pointDailyBossCloseReward, "pointDailyBossRewardClose1", sleepDuration = 500)
                tap(location = pointDailyBossCloseReward, "pointDailyBossRewardClose2", sleepDuration = sleepAfterFight) // loading after close reward
            }

            // Start by trying to finish 25 now. If that ever will become available
            //            repeat(3) {
            //                fight(pointDailyBoss25FinishNow_WhenMaxed, pointDailyBoss25FinishNow_WhenMaxed, "FinishNow", false)
            //            }

            // THIS IS all a mess, its from old version, and tries to work both for players who are maxed and players who are not yet maxed without configuration.

            // Finish off with finishNow's in case there are no good dragons selected in team
            val pEnterCurrentLevelWhenMaxed = if (player.dailyBossDoMax30) pointDailyBossEnterHighest_WhenMaxed else pointDailyBossEnterSecondHighest_WhenMaxed
            if (tryUncompleted) {
                fight(pointDailyBossEnterNextUncompletedChallenge_WhenUnmaxed, pointDailyBossEnterHighest_WhenMaxed, s"Enter Uncompleted", true)
            } else {
                fight(pointDailyBossEnterHighestCompleted_WhenUnmaxed, pointDailyBossEnterHighestCompleted_WhenMaxed, s"Enter completed", true)
            }
            fight(pointDailyBossEnterHighestCompleted_WhenUnmaxed, pEnterCurrentLevelWhenMaxed, s"EnterCurrentLevel", true)
            fight(pointDailyBossEnterHighestCompleted_WhenUnmaxed, pEnterCurrentLevelWhenMaxed, s"EnterCurrentLevel", true)
            repeat(3) {
                fight(pointDailyBossFinishNow_WhenUnmaxed, pointDailyBossFinishHighest_WhenMaxed, "25FinishNow", false)
            }
            if (player.dailyBossDoMax30) {
                repeat(3) {
                    fight(pointDailyBossFinishNow_WhenUnmaxed, pointDailyBossFinishSecondHighest_WhenMaxed, "24FinishNow", false)
                }
                repeat(3) {
                    fight(pointDailyBossFinishNow_WhenUnmaxed, pointDailyBossFinishThirdHighest_WhenMaxed, "23FinishNow", false)
                }
            }
            tap(location = pointDailyBossCloseReward, "pointDailyBossRewardClose", sleepDuration = 5 * MILLI_PR_SEC) // loading after close reward
            // If there are no more fights, we are at inner screen, otherwise outer. Hence do two closes
            tap(location = pointDailyBossClose, "pointDailyBossClose", sleepDuration = sleepDurationMenu, 3)
        }
    }

    def doBattleJourney() = {
        if (doLog) println(s"$pName BattleJourney")
        val journeyFightDuration = 85 * MILLI_PR_SEC
        val afterPlayRewardDuration = 2 * MILLI_PR_SEC
        val rewardDuration = 7 * MILLI_PR_SEC
        tap(location = pointMenu, "Menu", sleepDuration = sleepDurationMenu)
        tap(location = pointMenuBattle, "MenuBattle", sleepDuration = sleepDurationMenu)
        tap(location = pointMenuBattleJourney, "MenuBattleJourney", sleepDuration = sleepDurationMenu)

        def fight(p: Point, info: String): Unit = {
            screenshot("Journey_Info Start")
            for (_ <- 1 to 15) { // Game doesn't register click when changing level decade (xxx1)
                tap(location = p, s"Journey${info}", sleepDuration = sleepDurationMenuShort)
                tap(location = pointJourneyFight, s"JourneyFight$info", sleepDuration = sleepDurationMenuShort)
            }
            sleep(journeyFightDuration)
            // It takes 10 sec load after closing reward, so better to fight shorter time and repeat some closes
            tap(location = pointJourneyFightClose, s"JourneyFightClose", sleepDuration = afterPlayRewardDuration, repeat = 15)
        }

        fight(pointJourneySoulStone, "SoulStone")
        fight(pointJourneyGold, "Gold")
        fight(pointJourneyLifeStone, "LifeStone")
        for (i <- 1 to 2) {
            val timePassed = tap(location = pointJourneyRewardOpen, repeat = 5, sleepDuration = sleepDurationMenuShort)
            sleep(9000 - timePassed) // Takes 6 second to open, when running normally, we don't know which click above opened it due to repeats
            screenshot(s"Journey_Reward_$i")
        }
        tap(location = pointJourneyClose, sleepDuration = sleepDurationMenu)
    }

    def doSearchDragon(count: Int = 3) = {
        val tameDuration = 6000
        val closeDuration = 1500
        tap(location = pointMenu, sleepDuration = sleepDurationMenu)
        tap(location = pointMenuExplore, sleepDuration = sleepDurationMenu)
        tap(location = pointMenuExploreSearch, sleepDuration = sleepDurationMenu)

        tap(location = pointSearchEssenceTab, sleepDuration = sleepDurationMenuShort, repeat = 2)
        repeat(4) {
            def purchase(p: Point): Unit = {
                tap(location = p, sleepDuration = sleepDurationMenuShort)
                tap(location = pointSearchEssenceCollectPurchase, sleepDuration = sleepDurationMenuShort)
                // Its safe to close popup because its a dead-center click. Any off center opens dialog that screws things
                tap(location = pointSearchEssenceClosePopup, sleepDuration = sleepDurationMenuShort)
            }

            purchase(pointSearchEssenceFree)
            purchase(pointSearchEssenceGold)
            purchase(pointSearchEssenceFree) // Repeat not necessary if I got the next->dialog closes correctly
            tap(location = pointSearchNext, sleepDuration = sleepDurationMenuShort)
            // If no dialogs get shown, we have changed to next tier and just have to close the popup at the end
            // If dialogs are shown they prevent clicking on essences, so no need to close popups in-between
            // Close 'A high-tier essence' dialog + the popup that comes due to click essence when the warning is not there
            tap(location = pointSearchNextWarningOk, sleepDuration = sleepDurationMenuShort)
            // Close ads first
            tap(location = pointSearchNextWarningNoMoreSearchAndAd_SelectCancel, sleepDuration = sleepDurationMenuShort)
            tap(location = pointSearchNextWarningOk, sleepDuration = sleepDurationMenu)
            tap(location = pointSearchEssenceClosePopup, sleepDuration = sleepDurationMenuShort) // Finsish by closing
        }

        tap(location = pointSearchDragonTab, sleepDuration = sleepDurationMenuShort, repeat = 2)
        repeat(count) {
            tap(location = pointSearchNext, sleepDuration = sleepDurationMenu)
            tap(location = pointSearchNextWarningNoMoreSearchAndAd_SelectCancel, sleepDuration = sleepDurationMenuShort)
            tap(location = pointSearchNextWarningCancel, sleepDuration = sleepDurationMenuShort)

            def tame(): Unit = {
                tap(location = pointSearchDragonTameAgain, sleepDuration = tameDuration)
                tap(location = pointSearchDragonTameClose, sleepDuration = closeDuration)
                tap(location = pointSearchDragonTameClose, sleepDuration = closeDuration)
            }

            tap(location = pointSearchDragonTame1, sleepDuration = tameDuration)
            tame()
            tap(location = pointSearchDragonTame2, sleepDuration = tameDuration)
            tame()
        }
        tap(location = pointSearchNextWarningNoMoreSearchAndAd_SelectCancel, sleepDuration = sleepDurationMenuShort)
        tap(pointSearchClose, sleepDuration = sleepDurationMenu)

        for (i <- 1 to 2) {
            if (doLog) println(s"Search drag iteration  $i")
            doGridFromPointsToFunc(rectangleDragonGiftSpawn, 5, 3, innerFunc = dragInToCenter(_))
        }
    }

    def doSearchDimensionGate(count: Int = 2) = {
        tap(location = pointMenu, sleepDuration = sleepDurationMenu)
        tap(location = pointMenuExplore, sleepDuration = sleepDurationMenu)
        tap(location = pointMenuExploreDimGate, sleepDuration = sleepDurationMenu)

        tap(location = pointSearchDimensionGateInitialRift, sleepDuration = sleepDurationMenuShort, repeat = 2)

        repeat(count) {
            tap(location = pointSearchDimensionGateChooseEnter10X, sleepDuration = sleepDurationMenuShort)
            tap(location = pointSearchDimensionGateChooseEnterDimension, sleepDuration = sleepDurationMenuShort)
            tap(location = pointSearchDimensionGateChooseEnter5X, sleepDuration = sleepDurationMenuShort)
            tap(location = pointSearchDimensionGateChooseEnterDimension, sleepDuration = sleepDurationMenuShort, repeat = 2)
            tap(location = pointSearchDimensionGateChooseEnter1X, sleepDuration = sleepDurationMenuShort)
            tap(location = pointSearchDimensionGateChooseEnterDimension, sleepDuration = sleepDurationMenuShort, repeat = 4) // it sometimes pauses with loading animation
            // seems
            tap(location = pointSearchDimensionGateChooseChallengeAfterEnter, sleepDuration = sleepDurationMenuShort, repeat = 2)
            sleep(4000)
            tap(location = pointSearchDimensionGateSkipEffects, sleepDuration = sleepDurationMenuShort, repeat = 8) // Also closes reward
        }
        tap(pointSearchClose, sleepDuration = sleepDurationMenu)
    }

    def doChooseStage(stageType: StageType.Value, withScreenShots: Boolean) = {
        if (withScreenShots) {
            screenshot("ReturnViaMenu_ChooseStage_Before")
            sleep(2000) // Experiment, in case its the screenshot that cause delays in reaction to swipe
        }
        println(s"Choosing stage $stageType")
        // Double-swipe does not work, as they frequently both take effect, and it scrolls double or cancels scroll
        val sleepSwipeStage = 1500
        if (stageType == StageType.Wood) {
            println("Choosing Previous Stage")
            dragOnly(pointMainChooseType, pointMainChooseTypePrevious, swipeDurationMs = 500)
            sleep(sleepSwipeStage) // Some sleep is necessary
        } else {
            println("Choosing Next Stage")
            for (_ <- 0 until stageType.id) {
                println("Choosing Next Stage")
                dragOnly(pointMainChooseType, pointMainChooseTypeNext, swipeDurationMs = 500)
                sleep(sleepSwipeStage) // Some sleep is necessary
            }
        }
        if (withScreenShots) {
            screenshot("ReturnViaMenu_ChooseStage_AfterChoosing")
        }
        println("Tap choose stage")
        for (i <- 0 until 2) {
            tap(pointMainChooseType)
            sleep(500)
            tap(pointMainChooseTypeConfirm)
            if (i == 0) sleep(500)
        }
        println("Done choosing stage")
        sleep(60000)
    }

    def screenshot(name: String): Unit = {
        adb.screenshot(device.name, "_" + name)
    }

    def doReturn(viaType: ReturnType.Value, stageType: StageType.Value) = {
        println("Returning!")
        tap(pointCloseCommonNoPvpAutoDisable) // Close should other dialogs be open
        if (player.pvpAutoActive) {
            doPvpBattleAutoDisable()
            sleep(30000) // in case pvp was in progress
            doPvpBattleAutoDisable()
        }
        tap(pointNoToShinyFusingAndPvpQuitBattleDialog)
        val currentStageType = device.player.currentStageType
        // repeat in case of black screen changing stages. 3 times with 500 ms sleep was not enough, but seems to be just exactly enough in test, though sometimes menu clicking
        // fails, so use minimum 6
        for (i <- 1 to 10) {
            viaType match {
                case ReturnType.FailedStage =>
                    tap(pointFailedStageReturn, sleepDuration = 500)
                case ReturnType.Menu =>
                    // If menu is open, this closes, then taps nothing instead of return, and the following will open menu
                    println(s"Tapping menu -> return - $i")
                    device.player.setCurrentStageType(stageType)
                    tap(pointMenu, sleepDuration = 500)
                    tap(pointMenuReturn, sleepDuration = 500)
            }
        }
        val returnMethodName = viaType match {
            case ReturnType.FailedStage => "MaybeReturnViaFailedPopup"
            case ReturnType.Menu => "ReturnViaMenu_Reward_StartEnd"
        }
        screenshot(returnMethodName + "_" + currentStageType.map(_.toString()).getOrElse(Some("Unknown")))
        println("Tapping Return Yes+Yes")
        tap(pointReturnQ1Yes)
        sleep(1500) // Some sleep is necessary
        tap(pointReturnQ2Yes)
        println("Returning - done - will now sleep fro dragon flying animation")
        sleep(15000) // Dragons flying animation, black screen on 10s with 35% power
        doChooseStage(stageType, viaType == ReturnType.Menu)
        if (viaType == ReturnType.Menu) {
            for (i <- 1 to 2) {
                sleep(10000) // Loading new stage, so I don't get black screen, 8s is not enough probably in low power mode, 12s is enough in normal power but not in 35% cpu
                val postfix = if (i == 3) "StartEnd" else ""
                screenshot("Return done - new stage " + stageType + s"_${i}_$postfix")
            }
        }
        device.player.clearNextStageType()
        writePlayerFile()
    }

    private def doRush(idx: Int) = {
        val postfix = "_With_" + device.player.rushStagesRandom + "_RandomStages"
        println(s"Rush! $idx")
        screenshot("Rush" + idx)
        // Apparently it takes a couple of seconds to take a screenshot, if in nightly powersaving 60%, so I only have one chance at getting the right timing
        // 2x800 and 2x900 were to early
        tap(pointRush, sleepDuration = 1000)
        tap(pointRush, sleepDuration = 1000) // An extra tap in case we miss one
        screenshot("Rush-1600" + idx + postfix)
        repeat(2) {
            tap(pointRush, sleepDuration = 200)
        }
        // We may miss a rush due to screen loading, so try multiple times, 2x 500ms pause not enough
        sleep(13000) // It takes around 10s, on 35% power 12s was not enough, nor was 13s on 60%
        println("Rush - executed")
    }

    private def doRestartDragonSky() = {
        adb.restartDragonSky()
        // Takes 30sec to load game, and 20 sec to pop-up to appear
        sleep(30000)
        repeat(30) {
            tap(pointLoginClosePopUpWithProgressAfterReconnect, "ClosePopUpWithProgressAfterReconnect", 1000)
        }
    }

    def repeatForDuration(duration: Long, fn: () => Unit): Unit = {
        def time() = System.currentTimeMillis()

        val start = time()
        do {
            fn()
        } while (time() - start < duration && lock.isRunning())
    }

    def execute() {
        val player = device.player
        val screenReconnectTimer = MyTimer(16 * 60, "Reconnect").start()
        val afterReconnectClosePopUpTimer = MyTimer(1 * 60, "After Reconnect Close popup").start()
        if (!player.rushThenReturnTimer.isRunning && !player.finalStagesTimer.isRunning) {
            player.rushThenReturnTimer.start()
            player.rushThenReturnTimer.setNextDuration(player.playStageDuration)
        }
        println(s"Default next stage for ${device.name} is ${player.nextStageType()}")
        //dragDrawCorners(rectOuter)
        //dragDrawCorners("inner", rectInner)
        //dragDrawCorners("randomFrom", rectRandom)
        //        dragDrawCorners("randomTo", rectRandomTo)
        val loopTimer = MyTimer().start()
        while (!quit.get()) {
            try {
                while (!lock.isRunning()) {
                    isRunningInLoop.set(false)
                    lock.doWait()
                    player.restartDragonSkyTimer.restart()
                }
                if (quit.get()) return
                println(s"$pName Rush: ${player.getNextRushCount()}x")
                isRunningInLoop.set(true)
                // try/catch due to exception in line (probably from adb): 'heap corruption detected by dlfree'
                doPvpClose() // Conflicts testing return-choose-stage, when at choose stage
                if (player.screenShotTimer.isTimePassed()) {
                    // Also does screenshot
                    //                    doReturn(ReturnType.FailedStage, player.nextStageType())
                    screenshot("Stage_" + player.currentStageType.map(_.toString()).getOrElse(Some("Unknown")))
                    player.screenShotTimer.startNextLoop()
                }
                // Rush then immediately after check return timer, in case we don't want to farm extra final stages
                if (player.rushThenReturnTimer.isTimePassed()) {
                    val rushCount = player.getNextRushCountAndClear()
                    for (idx <- 1 to rushCount) doRush(idx)
                    sleep(10000)
                    screenshot("RushDone")
                    // now play normally for 5-15 minutes and then return, because we typically reach a higher level now
                    player.rushThenReturnTimer.stopAndReset()
                    player.finalStagesTimer.start()
                    writePlayerFile()
                }
                val doReturnNow = pendingReturnNow.getAndSet(false)
                if (player.finalStagesTimer.isRunning || doReturnNow) {
                    println(device.name + ": In progress with last stages after Rush before return, remaining " + player.finalStagesTimer.remainingToString())
                    if (player.finalStagesTimer.isTimePassed() || doReturnNow) {
                        val nextStageType = player.nextStageType()
                        player.finalStagesTimer.stopAndReset()
                        doReturn(ReturnType.Menu, nextStageType)
                        player.rushThenReturnTimer.start()
                        player.rushThenReturnTimer.setNextDuration(player.playStagesDurationForLevel(nextStageType))
                        player.finalStagesTimer.setNextDuration(player.finalStagesDurationForLevel(nextStageType))
                        player.screenShotTimer.restart() // Reset the time so it runs from after return, so we get screenshots every 15th minute from now
                        writePlayerFile()
                    }
                }
                if (screenReconnectTimer.isTimePassedWithRestart()) {
                    tap(pointReconnect, "Reconnect") // Reconnect takes nearly 30 sec
                    afterReconnectClosePopUpTimer.start()
                }
                if (afterReconnectClosePopUpTimer.isTimePassed()) {
                    // Required some seconds after reconnect, and its safe, so do it now and then
                    tap(pointLoginClosePopUpWithProgressAfterReconnect, "ClosePopUpWithProgressAfterReconnect")
                    afterReconnectClosePopUpTimer.stopAndReset()
                }
                if (player.autoPlaySearchDragons.isTimePassedWithRestart() || pendingSearchDragons.getAndSet(false)) {
                    writePlayerFile()
                    if (doLog) println(s"$pName SearchDragons")
                    doSearchDragon(player.dragonSearchCount.get())
                    player.dragonSearchCount.resetToMin()
                } else {
                    if (doLog) println(s"$pName SearchDragons ${player.autoPlaySearchDragons.remainingToString()}")
                }
                if (player.autoPlaySearchDimensionGate.isTimePassedWithRestart() || pendingDimensionGate.getAndSet(false)) {
                    writePlayerFile()
                    if (doLog) println(s"$pName SearchDimensionGate")
                    doSearchDimensionGate()
                }
                val midnight: Long = DateUtils.midnight().toEpochSecond(ZoneOffset.ofHours(0)) * 1000
                if (player.autoPlayDailyBoss.isTimePassed() || pendingDailyBoss.getAndSet(false)) {
                    player.autoPlayDailyBoss.restart()
                    if (doLog) println(s"$pName DailyBoss")
                    // Once par day try uncompleted (maybe?)
                    val tryUncompleted = player.autoPlayBattleLastDayTryUncompleted < midnight
                    if (tryUncompleted) player.autoPlayBattleLastDayTryUncompleted = midnight
                    writePlayerFile()
                    doBattleDailyBoss(tryUncompleted)
                } else {
                    if (doLog) println(s"$pName DailyBoss ${player.autoPlayDailyBoss.remainingToString()}")
                }
                // Don't start pvpAuto if returning soon
                val pvpAuto = pendingPvpAuto.getAndSet(false) ||
                    !player.pvpAutoActive && LocalDateTime.now().getHour == 23 &&
                        (!player.finalStagesTimer.isRunning || player.finalStagesTimer.remainingOrNextMin() > 15)
                val pvp = pendingPvp.getAndSet(false)
                if (player.autoPlayPvp.isTimePassed() || pvp || pvpAuto) {
                    player.autoPlayPvp.restart()
                    writePlayerFile()
                    doPvpBattle(pvpAuto)
                }
                if (player.pvpAutoActive && LocalDateTime.now().getHour == 0) {
                    doPvpBattleAutoDisable()
                }
                if (player.autoPlayJourneyLastDayCompleted < midnight && LocalDateTime.now().getHour >= hourToDoDailyJourneyHour || pendingJourney.getAndSet(false)) {
                    player.autoPlayJourneyLastDayCompleted = midnight
                    writePlayerFile()
                    doBattleJourney()
                } else {
                    if (doLog) println(s"$pName BattleJourney Last ${DateUtils.getDateAsString(new Date(player.autoPlayJourneyLastDayCompleted))}")
                }
                if (player.autoPlayLegendaryBoss.isTimePassed() || pendingLegendary.getAndSet(false)) {
                    player.autoPlayLegendaryBoss.restart()
                    writePlayerFile()
                    if (doLog) println(s"$pName LegendaryBoss")
                    doBattleLegendaryBoss() // Once par day try uncompleted (maybe?)
                } else {
                    if (doLog) println(s"$pName LegendaryBoss ${player.autoPlayLegendaryBoss.remainingToString()}")
                }
                if (player.restartDragonSkyTimer.isTimePassedWithRestart()) {
                    doRestartDragonSky()
                }

                // Drag in from egg in case dragon is stuck there
                dragForDragonMergeWithHatchAndMysticTaps(pointHatchEgg, rectRandomTo.random(), logInfo = "drag egg to dragons")
                for (_ <- 1 to 6) { // Mystic 6s pause at end means don't go to few times, one loop takes around 4 sec
                    // All current grid points are outside the random rect from so no value here
                    // doGridFromPointsToFunc(pointInnerStart, pointInnerEnd, 3, rectRandomFrom, dragInToRandom)
                    // Drag from outer corners
                    for (_ <- 1 to 1) dragForDragonMergeWithHatchAndMysticTaps(rectRandomFrom.randomWithBlacklist(rectRandomFromBlacklistCenter), rectRandomTo.random(), logInfo = "Random blacklist")
                    // allow dragging from top bottom center of screen, dragons seemed to get stuck there
                    for (_ <- 1 to 1) dragForDragonMergeWithHatchAndMysticTaps(rectRandomFrom.randomWithBlacklistY(rectRandomFromBlacklistCenter), rectRandomTo.random(), logInfo = "Random blacklist Y")
                    // random drags, by randomness mostly around the center
                    for (_ <- 1 to 2) dragForDragonMergeWithHatchAndMysticTaps(rectRandomFrom.random(), rectRandomTo.random(), logInfo = "Random from => to") // Avoid dragons getting stuck on top and bottom center
                    for (_ <- 1 to 2) dragForDragonMergeWithHatchAndMysticTaps(rectRandomTo.random(), rectRandomTo.random(), logInfo = "Random to => to")
                    tap(pointNoToShinyFusingAndPvpQuitBattleDialog)
                }
                if (doMysticTapAfterMergeDragons) repeatForDuration(6000, mysticUpgradeDragonMergeToLegendaryOrMystic)
                println(s"${device.name}: Loop: " + loopTimer.deltaDuration())
                loopTimer.start()
            } catch {
                case e: Throwable =>
                    println("Caught exception in execute")
                    e.printStackTrace()
            }
        }
    }

    private val thread = new Thread(() => {
        try {
            execute()
        } catch {
            case e: Exception => e.printStackTrace()
        }
    })
    thread.start()
}

def repeat(n: Int)(fn: => Unit) {
    for (_ <- 1 to n) fn
}

def loop(n: Int)(fn: => Unit) = repeat(n)(fn)

def retry[T](n: Int, duration: Int = 100, onErrorAction: () => Unit = () => {})(fn: => T): T = {
    try {
        fn
    } catch {
        // ammonite.ops.InteractiveShelloutException
        // ignore : java.lang.IllegalArgumentException: Zero lines, retry (devices in the startup
        case e: Throwable =>
            if (n > 1) {
                // TODO Logs nulls, but filter away adb
                e.printStackTrace()
                println("Retrying due to error: " + e.getClass + ": " + e.getMessage)
                onErrorAction()
                Thread.sleep(duration)
                retry(n - 1)(fn)
            } else {
                println("All retries failed, now propagating exception: " + e.getClass + ": " + e.getMessage)
                throw e
            }
    }
}

def execEngine(device: DeviceSerial, changesize: Boolean = false): Unit = {
    GameHackEngine(device, changesize).execute()
}


@main
def gui() {
    // Kill Adb frequently, because it hangs once a day or so and thread is thus stuck
    new KillAdbThread().startInNewThread()
    try {
        // Bluestack emulator connection
        Adb.disconnectMainPort()
    } catch {
        case _: Throwable =>
    }
    new Gui()
    val syncObj = new Object()
    syncObj.synchronized {
        syncObj.wait() // indefinite wait, like while(true) with cpu
    }
}

@main
def test() {
    def test(count: Int) {
        println(s"$count min: " + computeExpectedStageLevelsByDuration(count * 60) + " stages")
    }

    test(1)
    test(10)
    test(20)
    test(60)
    test(66)
    test(90)
    test(120)
    test(240)
    test(300)
    test(400)
    test(241)
    test(239)
}

@main
def any(changesize: Boolean@doc("change resolution on device to 720p") = false) {
    val devices: Seq[DeviceSerial] = retry(10) {
        Adb.devices()
    }
    if (devices.isEmpty) throw new RuntimeException("No devices found")
    if (devices.size > 1) throw new RuntimeException("More than one device attached, used 'index' command")
    execEngine(devices(0), changesize)
}

@main
def serial(serial: String@doc("serial, use adb devices for list of serials"),
           changesize: Boolean@doc("change resolution on device to 720p") = false) {
    execEngine(DeviceSerial(serial, "", ""), changesize)
}

@main
def index(deviceIndex: Int@doc("index in 'adb devices' list (0 is first device line)"),
          changesize: Boolean@doc("change resolution on device to 720p") = false) {
    val devices: Seq[DeviceSerial] = retry(10) {
        Adb.devices()
    }
    execEngine(devices(deviceIndex), changesize)
}

@main
def list() {
    println(%%("adb", "devices").out.string.trim)
}

@main
def reset(isPro: Boolean = true) {
    val serial = isPro match {
        case true => "ae3a7d7b"
    }
    val adb = Adb(serial)
    adb.adbShellExec("wm", "density", "600")
    adb.adbShellExec("wm", "size", "reset")
    adb.adbShellExec("wm", "density")
    adb.adbShellExec("wm", "size")
}
