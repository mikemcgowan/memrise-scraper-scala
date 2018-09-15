package mcgowan

import java.io.{BufferedWriter, File, FileWriter}

object Writer {
  def write(filename: String, header: String, rows: Seq[String]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(header)
    rows foreach bw.write
    bw.close()
  }
}
