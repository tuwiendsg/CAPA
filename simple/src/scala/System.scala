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
package simple

import amber.util.{EventSource, Logging}

trait System extends amber.System.Local
             with amber.origin.BuilderComponent.Local.Default
             with amber.origin.BuilderComponent.Logging.Local
             with amber.origin.FinderComponent.Local.Default
             with amber.family.FinderComponent.Default
             with amber.origin.FactoryComponent.Default
             with amber.family.MemberFactoryComponent.Default {
  this: Logging =>

  @transient private[this] val log = logger.create("amber.simple.System")

  override val stopped = EventSource[Unit]()
  override def origin: super.OriginFactory = _origin
  override protected def in(f: Origin.Family) = new MemberFactory with MemberFactory.Logging {
    override protected val family = f
    @transient override protected val log =
      logger.create(s"amber.simple.family.MemberFactory($family)")
  }

  override def shutdown() {
    log.info("Shutting down")
    super.shutdown()
    stopped.emit(())
    log.info("Shutdown successful")
  }

  trait OriginFactory extends super.OriginFactory with OriginFactory.Logging {
    @transient override protected val log = logger.create("amber.simple.origin.Factory")
  }

  private object _origin extends OriginFactory
}
