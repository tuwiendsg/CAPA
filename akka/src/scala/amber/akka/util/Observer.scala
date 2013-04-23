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

package amber
package akka
package util

import scala.reflect.ClassTag

import _root_.akka.actor.Actor

import amber.util.Events

private[akka] object Observer {

  class Notifier[+A](f: Events.Observe[A])(implicit classTag: ClassTag[A]) extends Actor {

    private val klass = classTag.runtimeClass

    override def receive = {
      case any if klass.isAssignableFrom(any.getClass) =>
        val a = any.asInstanceOf[A]
        if (f.isDefinedAt(a)) f(a)
    }
  }
}
