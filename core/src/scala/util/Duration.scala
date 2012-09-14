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
package util

import java.util.concurrent.TimeUnit
import TimeUnit._

case class Duration(length: Long, unit: TimeUnit) {
  def inNanoseconds = NANOSECONDS.convert(length, unit)
  def inMicroseconds = MICROSECONDS.convert(length, unit)
  def inMilliseconds = MILLISECONDS.convert(length, unit)
  def inSeconds = SECONDS.convert(length, unit)
  def inMinutes = MINUTES.convert(length, unit)
  def inHours = HOURS.convert(length, unit)
  def inDays = DAYS.convert(length, unit)
}

object Duration {

  case class LongDuration(length: Long) {
    def nanosecond = Duration(length, NANOSECONDS)
    def microsecond = Duration(length, MICROSECONDS)
    def millisecond = Duration(length, MILLISECONDS)
    def second = Duration(length, SECONDS)
    def minute = Duration(length, MINUTES)
    def hour = Duration(length, HOURS)
    def day = Duration(length, DAYS)

    def nanoseconds = Duration(length, NANOSECONDS)
    def microseconds = Duration(length, MICROSECONDS)
    def milliseconds = Duration(length, MILLISECONDS)
    def seconds = Duration(length, SECONDS)
    def minutes = Duration(length, MINUTES)
    def hours = Duration(length, HOURS)
    def days = Duration(length, DAYS)
  }

  trait Conversions {
    implicit def fromInt(length: Int) = LongDuration(length)
    implicit def fromLong(length: Long) = LongDuration(length)
  }

  object Conversions extends Conversions
}
