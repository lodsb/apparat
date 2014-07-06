/*
 +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2011 - 3 - 9 :: 10 : 35
    >>  Origin: mt4j (project) / mt4j_mod (module)
    >>
  +3>>
    >>  Copyright (c) 2011-2014:
    >>
    >>     |             |     |
    >>     |    ,---.,---|,---.|---.
    >>     |    |   ||   |`---.|   |
    >>     `---'`---'`---'`---'`---'
    >>                    // Niklas Klügel
    >>
  +4>>
    >>  Made in Bavaria by fat little elves - since 1983.
 */

package org.lodsb.apparat.midi

import org.lodsb.input._
import javax.sound.midi._
import javax.sound.midi.MidiDevice.Info
import org.lodsb.TraitOutputSink
import org.lodsb.apparat.{TraitOutputSink, TraitInputSource}

// FIXME, note and  controllers with int as well
// hide send and receive just expose API
abstract class AbstractMidiMsg {
	def channel: Int
}

abstract class AbstractNoteMsg extends AbstractMidiMsg{
	def note: Int
}

//case class MidiMsg(channel: Int) extends AbstractMidiMsg
case class MidiCtrlMsg(channel: Int, num: Int, data: Float) extends AbstractMidiMsg

case class MidiNoteOnMsg(channel: Int, note: Int, velocity: Float) extends AbstractMidiMsg

case class MidiNoteOffMsg(channel: Int, note: Int) extends AbstractMidiMsg

case class MidiGenericMessage(channel: Int, command: Int, data1: Byte, data2: Byte) extends AbstractMidiMsg

trait TraitMidiInputSource extends TraitInputSource[AbstractMidiMsg, Nothing]
trait TraitMidiOutputSink extends TraitOutputSink[AbstractMidiMsg]

object MidiCommunication {

	class MidiInput extends Receiver with TraitMidiInputSource {
		def close {
			println("Input closed")
		}

		def send(m: MidiMessage, time: Long) {
			m match {
				case sm: ShortMessage if (sm.getCommand() == ShortMessage.CONTROL_CHANGE) => {
					val ch = sm.getChannel()
					val num = sm.getData1()
					val v = (sm.getData2().floatValue)/127.0f

					this.receipt.emit(MidiCtrlMsg(ch,num,v))
				}
				
				case sm: ShortMessage if (sm.getCommand() == ShortMessage.NOTE_OFF) => {
					val ch = sm.getChannel()
					val note = sm.getData1()

					this.receipt.emit(MidiNoteOffMsg(ch, note))
				}

				case sm: ShortMessage if (sm.getCommand() == ShortMessage.NOTE_ON) => {
					val ch = sm.getChannel()
					val note = sm.getData1()
					val velocity = (sm.getData2().floatValue) / 127.0f

					// note off via velocity = 0
					if(velocity == 0.0f) {
						this.receipt.emit(MidiNoteOffMsg(ch, note))
					} else {
						this.receipt.emit(MidiNoteOnMsg(ch, note, velocity))
					}
				}

				case _=> println("Message not supported")
			}
		}
	}

	class MidiOutput(private val receiver: Receiver) extends TraitMidiOutputSink {
		def senderAction(message: AbstractMidiMsg): Boolean =  {
			message match {
        case MidiGenericMessage(cha, cmd, d1, d2) => {
          val msg = new ShortMessage
          msg.setMessage(cmd, cha, d1, d2)
          this.receiver.send(msg,-1)
        }

				case MidiCtrlMsg(cha, num, v) => {
					val msg = new ShortMessage
					val value = (v*127.0f).toInt
					msg.setMessage(ShortMessage.CONTROL_CHANGE, cha, num, value);
					this.receiver.send(msg, -1)
				}

        case MidiNoteOnMsg(channel: Int, note: Int, velocity: Float) => {
          val msg = new ShortMessage
          val velo = (velocity*127.0f).toInt
          msg.setMessage(ShortMessage.NOTE_ON, channel, note, velo)
          this.receiver.send(msg, -1)
        }

        case MidiNoteOffMsg(channel: Int, note: Int) => {
          val msg = new ShortMessage
          msg.setMessage(ShortMessage.NOTE_OFF, channel, note, 0)
          this.receiver.send(msg, -1)
        }


        case _ => println("This MidiMessage-Type is NOT YET SUPPORTED")
			}

			true
		}

		this.sendAction = senderAction
	}

	def knownDevices = MidiSystem.getMidiDeviceInfo

	def createMidiInput(deviceName: String): Option[MidiInput] = {
		val DUMP_IN = false
		val DUMP_OUT = false
		val infos: Array[Info] = knownDevices

    printDeviceInfos(infos)

		val inDevO = infos.filter(_.getName.contains(deviceName)).map(MidiSystem.getMidiDevice(_)).find(_.getMaxTransmitters() != 0)

		val ret = openDeviceForInput(inDevO)

		if(ret.isEmpty) {
			println("No input device '" + deviceName + "' found!")
		}

		ret
	}

  private def printDeviceInfos(infos: Array[Info]) = {
    println("Midi Devices:")
    infos.foreach({x =>
      val dev = MidiSystem.getMidiDevice(x)
      println("\tFound MIDI-Device: " +
        "\n\t\tDescription: "+x.getDescription+ "" +
        "\n\t\tName: '"+x.getName+
        "'\n\t\tVersion: "+x.getVersion+
        "'\n\t\tOuts: "+dev.getMaxReceivers+
        "'\n\t\tIns: "+dev.getMaxReceivers
      )
    })
  }

	def createMidiInputByDeviceIndex(index: Int) : Option[MidiInput] = {
		
		val infos = knownDevices

		if(index < knownDevices.size) {
			val deviceInfo = infos(index)

			println("Opening "+deviceInfo)
			openDeviceForInput(Some(MidiSystem.getMidiDevice(deviceInfo)))
		} else {
			None
		}
	}

	private def openDeviceForInput(inDevO: Option[MidiDevice]) : Option[MidiInput] = {
		var ret: Option[MidiInput] = None;

		try {
			inDevO match {
				case Some(inDev) =>
					inDev.open
					val t = inDev.getTransmitter()
					val input = new MidiInput
					t.setReceiver(input)
					ret = Some(input)
				case _  =>
			}
		} catch {
			case e: Throwable =>
				println("Error initializing MIDI (Input): ")
				e.printStackTrace()
		}

		ret
	}

  private def openDeviceForOutput(outDevO: Option[MidiDevice]) : Option[MidiOutput] = {
    var ret: Option[MidiOutput] = None;

    try {
      outDevO match {
        case Some(outDev) =>
          outDev.open
          val output = new MidiOutput(outDev.getReceiver)

          ret = Some(output)

        case _ =>
          //if (outDevO.isEmpty) println("No output device '" + deviceName + "' found!")
      }
    } catch {
      case e:Throwable =>
        println("Error initializing MIDI (Output): ")
        e.printStackTrace()
    }

    ret
  }

	def createMidiOutput(deviceName: String): Option[MidiOutput] = {
		val DUMP_IN = false
		val DUMP_OUT = false
		val infos = knownDevices

    printDeviceInfos(infos)

		var ret: Option[MidiOutput] = None;

		infos.foreach(x => println(x.getDescription))

		val outDevO = infos.filter(_.getName.contains(deviceName)).map(MidiSystem.getMidiDevice(_)).find(_.getMaxReceivers() != 0)
    ret = openDeviceForOutput(outDevO)

    if(ret.isEmpty) {
      println("No output device '" + deviceName + "' found!")
    }

		ret
	}



  def createMidiOutputByDeviceIndex(index: Int) : Option[MidiOutput] = {

    val infos = knownDevices

    if(index < knownDevices.size) {
      val deviceInfo = infos(index)

      println("Opening "+deviceInfo)
      openDeviceForOutput(Some(MidiSystem.getMidiDevice(deviceInfo)))
    } else {
      None
    }
  }
}
