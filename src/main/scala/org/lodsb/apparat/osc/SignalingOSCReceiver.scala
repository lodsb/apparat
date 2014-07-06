/*
 +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2011 - 3 - 9 :: 10 : 26
    >>  Origin: mt4j (project) / mt4j_mod (module)
    >>
  +3>>
    >>  Copyright (c) 2011:
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

package org.lodsb.apparat.osc

import java.nio.channels.DatagramChannel
import de.sciss.osc._
import impl.UndirectedNetInputImpl
import java.net.SocketAddress
import de.sciss.osc.Channel.Directed.Input

class SignalingOSCReceiver(val receiver: de.sciss.osc.Channel) extends TraitReceiveOSCComm  {
  def close() = receiver.close()
  def isConnected = receiver.isConnected
}








