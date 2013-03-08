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

import amber.util.{ConfigurableComponent, Logging}
import akka.util.EventSource

trait System extends amber.System
             with akka.origin.BuilderComponent
             with amber.origin.BuilderComponent.Logging
             with amber.origin.FinderComponent.Local.Default
             with amber.family.FinderComponent.Default
             with amber.origin.FactoryComponent.Default
             with amber.family.MemberFactoryComponent.Default
             with ConfigurableComponent {
  this: Logging =>

  override protected type Configuration = System.Configuration

  @transient private[this] val log = logger.create("amber.akka.System")

  override val stopped = EventSource[Unit](configuration.system)
  override def origin: super.OriginFactory = _origin
  override protected def in(f: amber.Origin.Family) = new MemberFactory with MemberFactory.Logging {
    override protected val family = f
    @transient override protected val log =
      logger.create(s"amber.simple.family.MemberFactory($family)")
  }

  override def shutdown() {
    log.info("Shutting down")
    super.shutdown()
    stopped emit ()
    log.info("Shutdown successful")
  }

  trait OriginFactory extends super.OriginFactory with OriginFactory.Logging {
    @transient override protected val log = logger.create("amber.akka.origin.Factory")
    override val created = EventSource[Origin[_]](configuration.system)
  }

  private object _origin extends OriginFactory
}

object System {
  trait Configuration extends origin.BuilderComponent.Configuration
}
