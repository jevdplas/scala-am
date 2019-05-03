package scalaam.bench

import java.io.FileWriter

// Based on https://github.com/acieroid/scala-am/tree/modularthreads
class Recorder[TID, Addr, Abs]{
    
    class Recorded[V](val recordName: String, val toStr: V => String = (v: V) => v.toString) {
        var content: Map[TID, Set[V]] = Map.empty.withDefaultValue(Set.empty)
        def record(t: TID, v: V) =
            content = content + (t -> (content(t) + v))
        def reset(): Unit = content = Map.empty.withDefaultValue(Set.empty)
        def keys: Set[TID] = content.keySet
        def report(t: TID): String = content(t).map(v => s"($recordName ${toStr(v)})").mkString(" ")
    }
    
    object Recorded {
        def empty[V](recordName: String): Recorded[V] = new Recorded(recordName)
        def emptyF[V](recordName: String, toStr: V => String): Recorded[V] = new Recorded(recordName, toStr)
    }
    
    val recordedCreate: Recorded[TID] = Recorded.empty[TID]("create")
    val recordedJoin: Recorded[TID] = Recorded.empty[TID]("join")
    val recordedRead: Recorded[(Addr, Abs)] = Recorded.emptyF[(Addr, Abs)]("read", { case (a, v) => s"$a $v" })
    val recordedWrite: Recorded[(Addr, Abs)] = Recorded.emptyF[(Addr, Abs)]("write", { case (a, v) => s"$a $v" })
    
    def outputRecorded(file: String): Unit = {
        def keys = recordedCreate.keys ++ recordedJoin.keys ++ recordedRead.keys ++ recordedWrite.keys
        val fw = new FileWriter("./recordings/" ++ file)
        try {
            keys.foreach(k =>
                fw.append(s"($k (${recordedCreate.report(k)} ${recordedJoin.report(k)} ${recordedRead.report(k)} ${recordedWrite.report(k)}))".filter(_ != '#')))
        } catch {
            case e: Throwable => e.printStackTrace()
        }
        fw.close()
    }
    
    def reset(): Unit = {
        recordedCreate.reset()
        recordedJoin.reset()
        recordedRead.reset()
        recordedWrite.reset()
    }
}
