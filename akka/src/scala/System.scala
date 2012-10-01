/*
 * Copyright 2012 Sanjin Sehic
 *

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
package akka

import amber.util.Logging
import amber.util.Path.Root
import akka.util.EventSource

trait System extends akka.origin.BuilderComponent
             with amber.System
             with amber.origin.FinderComponent.Default
             with amber.family.FinderComponent.Default
             with amber.origin.FactoryComponent.Default
             with amber.family.MemberFactoryComponent.Default {
  this: Logging =>

  @transient private[this] val log = logger.create("amber.akka.System")

  override val stopped = EventSource[Unit]()
  override def origin: super.OriginFactory = _origin
  override protected def origins: OriginFinder = _origins
  override protected def in(f: Origin.Family) = new MemberFactory with MemberFactory.Logging {
    override protected val family = f
    @transient override protected val log =
      logger.create("amber.simple.family.MemberFactory(" + family + ")")
  }

  override def shutdown() {
    log.info("Shutting down")
    super.shutdown()
    origins.killAll()
    stopped emit ()
    log.info("Shutdown successful")
  }

  trait OriginFactory extends super.OriginFactory with OriginFactory.Logging {
    @transient override protected val log = logger.create("amber.akka.origin.Factory")
    override val created = EventSource[Origin[_]]()
  }

  protected trait OriginFinder extends super.OriginFinder {
    def killAll() {
      log.debug("Killing all origins")
      val origins = find(Root)
      origins foreach {_.kill()}
      log.debug("Killed " + origins.size + " origins")
    }
  }

  private object _origin extends OriginFactory
  private object _origins extends OriginFinder
}
