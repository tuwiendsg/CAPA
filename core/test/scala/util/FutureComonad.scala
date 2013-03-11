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

import scala.concurrent.{future, Await, ExecutionContext, Future}
import scala.concurrent.duration._

import scalaz.Comonad

trait FutureComonad {

  implicit def context: ExecutionContext

  def timeout: FiniteDuration = 1.second

  implicit object FutureIsComonad extends Comonad[Future] {
    def copoint[A](fa: Future[A]): A = Await.result(fa, timeout)
    def cobind[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = future {f(fa)}
    def cojoin[A](fa: Future[A]): Future[Future[A]] = future {fa}
    def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }
}
