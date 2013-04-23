/*
 * Copyright 2013 Sanjin Sehic
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

import scala.concurrent.{ExecutionContext, Future}

import scalaz.Monad

package object amber {

  implicit def futureMonad(implicit context: ExecutionContext): Monad[Future] = new FutureMonad

  class FutureMonad(implicit context: ExecutionContext) extends Monad[Future] {
    override def point[A](a: => A) = Future.successful(a)
    override def map[A, B](future: Future[A])(f: A => B) = future.map(f)
    override def bind[A, B](future: Future[A])(f: A => Future[B]) = future.flatMap(f)
  }
}
