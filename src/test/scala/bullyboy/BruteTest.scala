/*
 * Copyright 2016 David Crosson
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package bullyboy

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.ShouldMatchers
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BruteTest extends FunSuite with ShouldMatchers {
  
  test("Simple test") {
    info("Test done")
  }
  
  
  test("Password generator") {
    val alphabet=Alphabet(Seq('A', 'B'))
    
    val brut1 = new PasswordGenerator(GoalContext(1, alphabet))
    brut1.generator().toList should have size(2)
    
    val brut2 = new PasswordGenerator(GoalContext(2, alphabet))
    brut2.generator().toList should have size(4)
  }
}
