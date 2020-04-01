
class AtomicRangedInteger(val min: Int, val max: Int, val incrementSize: Int = 1) {
    if (min > max) throw new IllegalArgumentException()
    if (increment > max - min) throw new IllegalArgumentException()
    var value: Int = min

    def get(): Int =
        this.synchronized {
            value
        }

    def increment(): Int = {
        this.synchronized {
            value = value + incrementSize
            if (value > max) value = min
            value
        }
    }

    def resetToMin(): Int = set(min)

    def set(newValue: Int): Int = {
        this.synchronized {
            val old = value
            value = newValue
            old
        }
    }
}