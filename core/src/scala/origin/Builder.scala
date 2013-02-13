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
package origin

trait BuilderComponent {

  protected type Origin[+A] <: amber.Origin[A]
  protected def builder: OriginBuilder

  protected trait OriginBuilder {
    def build[A: Manifest, B: Origin.Read[A]#apply](name: Origin.Name,
                                                    family: Origin.Family,
                                                    read: B): Origin[A]
  }
}
