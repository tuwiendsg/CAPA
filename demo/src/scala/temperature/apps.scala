/*
 * Copyright 2012 Sanjin Sehic
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package at.ac.tuwien.infosys
package amber
package demo
package temperature

import com.typesafe.config.ConfigFactory

import _root_.akka.actor.ActorSystem

object Simple extends Demo.Local with App with util.SLF4JLogging {

  override object system extends simple.System with super.System

  run()
}

object AkkaLocal extends Demo.Local with App with util.SLF4JLogging {

  override object system extends akka.System.Local with super.System {

    override protected object configuration extends akka.System.Local.Configuration {
      private val name = "temperature-server"
      override val system = ActorSystem(name, ConfigFactory.load.getConfig(name))
    }

    override def shutdown() {
      super.shutdown()
      configuration.system.shutdown()
    }
  }

  run()
}

object AkkaRemote extends Demo.Remote with App with util.SLF4JLogging {

  override object system extends akka.System.Remote with super.System {

    override protected object configuration extends akka.System.Remote.Configuration {
      private val name = "temperature-client"
      override val system = ActorSystem(name, ConfigFactory.load.getConfig(name))
      override val reference =
        system.actorFor("akka://temperature-server@127.0.0.1:2552/user/origins-finder")
    }

    override def shutdown() {
      super.shutdown()
      configuration.system.shutdown()
    }
  }

  try {
    AkkaLocal.system.start()
    run()
  } finally {
    shutdown()
    AkkaLocal.system.shutdown()
  }
}
