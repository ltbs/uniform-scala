package ltbs.uniform
package interpreter.asterisk

import java.io.File
import marytts._, util.data.audio._

object SpeechSynthesis {

  lazy private val mary = new LocalMaryInterface()

  def toWavFile(text: String, filename: String): File = {
    val audio = mary.generateAudio(text)
    val samples = MaryAudioUtils.getSamplesAsDoubleArray(audio)
    MaryAudioUtils.writeWavFile(samples, filename, audio.getFormat())
    new File(filename)
  }

  def toWavFile(text: String): File =
    toWavFile(text, s"/tmp/$text.wav")

}
