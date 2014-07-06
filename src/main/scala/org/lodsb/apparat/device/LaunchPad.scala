package org.lodsb.apparat.device

import scala.collection.mutable.ArrayBuffer
import org.lodsb.apparat.midi.{MidiCtrlMsg, MidiNoteOffMsg, MidiNoteOnMsg, MidiCommunication}
import scala.util.Random
import org.lodsb.apparat.midi.MidiCommunication.{MidiInput, MidiOutput}
import javax.sound.midi.MidiSystem
import org.lodsb.apparat.device.Page.LedColor

/**
 * Created by lodsb on 1/12/14.
 */

object Page {
  type LedColor = Int

  val Off : LedColor = 0
  val RedLow : LedColor = 13
  val RedFull : LedColor = 15
  val RedFlash : LedColor = 11
  val AmberLow : LedColor = 29
  val AmberFull : LedColor = 63
  val AmberFlash : LedColor = 59
  val YellowFull : LedColor = 62
  val YellowFlash : LedColor = 58
  val GreenLow : LedColor = 28
  val GreenFull : LedColor = 60
  val GreenFlash : LedColor = 56

  def apply(size: Int) = new Page(size)
}

class Page(size: Int) {

  private val xyGrid = ArrayBuffer.fill(8*size)(0)
  private val sceneGrid = ArrayBuffer.fill(8)(0)
  private val automapGrid = ArrayBuffer.fill(8)(0)

  private var changed = false

  private var startLine = 0;
  def scroll(numRows : Int) = {}

  def setGrid(x: Int, y: Int, color: LedColor) = {
    if(y < size && x < 8) {
      xyGrid(x+(y*8)) = color

      changed = true
    }
  }

  def setScene(which: Int, color: LedColor) = {
    if(which < 8) {
      sceneGrid(which) = color

      changed = true
    }
  }
  def setAutoMap(which: Int, color: LedColor) = {
    if(which < 8) {
      automapGrid(which) = color

      changed = true
    }
  }

  protected[LaunchPad] def getLedMatrix : ArrayBuffer[Int] = {
    val start = 8*startLine
    val xySlice = xyGrid.slice(start, start + 64)
    xySlice ++ sceneGrid ++ automapGrid
  }

  protected[LaunchPad] def isChanged = changed

  protected[LaunchPad] def setChanged(c: Boolean) = {changed = c}

  protected[LaunchPad] def gridButtonPressed(x: Int, y: Int, pressed: Boolean) = {println("grid "+x+ " "+y + " "+pressed)}
  protected[LaunchPad] def sceneButtonPressed(x: Int, pressed: Boolean) = {println("scene "+x+ " p "+pressed)}
  protected[LaunchPad] def automapButtonPressed(x: Int, pressed: Boolean) = {println("automap "+x+ " "+pressed)}
}

class LaunchPad(devicename: String = "Launchpad") {
  // 8x8 grid + two sets of buttons for automap and scene launch
  // format is led value and dirty flag

  // like the internal layout, first 64 slots are xy
  // next 8 scene launch
  // next 8 controls
  private val lights = ArrayBuffer.fill(10*8)( (0,false) )

  private var input: Option[MidiInput] = None
  private var output: Option[MidiOutput] = None

  input = MidiCommunication.createMidiInput(devicename)
  output = MidiCommunication.createMidiOutput(devicename)

  if(input.isDefined) {
    input.get.receipt.observe({x =>

        x match {
          case MidiNoteOnMsg(x,y,z) => handleGridAndSceneButtonPress(y,true)
          case MidiNoteOffMsg(x,y) => handleGridAndSceneButtonPress(y,false)
          case MidiCtrlMsg(x,y,z) => handleAutoMapButtonPress(y,z)
          case _ =>
        }


    true})
  }

  // enable led flashing
  if(output.isDefined) {
    output.get.send() = MidiNoteOnMsg(0, 0, 40.0f/127.0f )
  }


  private var currentPage : Option[Page] = None

  def setCurrentPage(page: Page) = {
    currentPage = Some(page)
  }

  private def handleAutoMapButtonPress(position: Int, pressed: Float) = {
    if(currentPage.isDefined) {
      val page = currentPage.get

      page.automapButtonPressed(position-104, pressed == 1.0f)
    }
  }

  private def handleGridAndSceneButtonPress(position: Int, pressed: Boolean) = {
    if(currentPage.isDefined) {
      val page = currentPage.get
      val div = position / 8
      val mod = position % 8
      if(position >= 8 && mod == 0 && div % 2 != 0) {
        page.sceneButtonPressed((div-1)/2, pressed)
      } else {
        val x = position % 8
        val y = position / 8

        page.gridButtonPressed(x,y/2,pressed)
      }
    }
  }

  private def updateLightsArray(page: Page) = {
    val arr = page.getLedMatrix

    for(i <- 0 to lights.size -1) {
      if(arr(i) != lights(i)._1) {
        lights(i) = (arr(i), true)
      }
    }
  }

  private def updateLEDs = {
    // use smart update, dirty flags? if < 40 leds to update then use simple midi
    // using rapid LED update:
    // note on, key&velo are the encoding for led status in pairs of two

    if(output.isDefined && currentPage.isDefined && currentPage.get.isChanged) {
      val page = currentPage.get
      page.setChanged(false)

      updateLightsArray(page)

      val midiOutput = output.get

      val numDirty = lights.filter( x=> x._2).length

      // reset cursor
      midiOutput.send() = MidiNoteOffMsg(0, 0)

      if(numDirty < 40) {
        lights.zipWithIndex.foreach { x=>
          val idx = x._2
          val t = x._1

          if(t._2) {
            val data = t._1/127.0f
            if(idx < 64) {
              val x = idx % 8
              val y = idx / 8
              val midiNote = (y*16)+x
              midiOutput.send() = MidiNoteOnMsg(0, midiNote, data)
            } else if(idx >= 64 && idx < 72) {
              val sceneNo = idx - 64
              midiOutput.send() = MidiNoteOnMsg(0, sceneNo*16 + 8, data)
            } else {
              val autoMapLed = idx - 72
              midiOutput.send() = MidiCtrlMsg(0, autoMapLed+104, data)
            }

            lights(idx) = (t._1, false)
          }
        }
      }  else {
        for(i <- 0 to (lights.length/2)-2) {
            val idx = i*2
            val velo1 = lights(idx)._1
            val velo2 = lights(idx+1)._1/127.0f

          lights(idx) = ( lights(idx)._1 , false)
          lights(idx+1) = ( lights(idx+1)._1 , false)

            midiOutput.send() = MidiNoteOnMsg(2, velo1, velo2)
            //midiOutput.send() = MidiNoteOnMsg(1, 57, 60)
        }
      }
    }
  }

  private val rand = new Random()

  private def randomizePage = {

    for(i <- 0 to lights.length-1) {
        if(rand.nextFloat() >= 0.8) {
          val value =  scala.math.abs(rand.nextInt()%127)

          if(currentPage.isDefined) {
            val page = currentPage.get

            page.setGrid(scala.math.abs(rand.nextInt()%8), (scala.math.abs(rand.nextInt()%64)), value.asInstanceOf[LedColor])
            page.setAutoMap(scala.math.abs(rand.nextInt()%8),Page.GreenFlash)
            page.setScene(scala.math.abs(rand.nextInt()%8),Page.AmberFlash)
          }
        }
      }
  }

  private val updateThread = new Thread( new Runnable {
    def run(): Unit = {
      while(true) {
        //randomizePage
        updateLEDs
        // max update time is 400ms
        Thread.sleep(200)
      }
    }
  })


  updateThread.start()
/*
  def main(args: Array[String]) {

    val page = new Page(8)
    setCurrentPage(page)

    updateThread.start()
  }
*/
}
