//package gsd.linux.bfm
//
//import org.junit.Test
//import org.scalatest.junit.AssertionsForJUnit
//
//import BFMTranslation._
//
//class ClaferTest extends AssertionsForJUnit with ClaferDocument {
//  @Test
//  def dead {
//    val k = KConfigParser.parseKConfig(
//      """
//      config A boolean
//      config B boolean
//      config C boolean
//      config D boolean
//      config E boolean
//      """)
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).format(System.out)
//  }
//
//  @Test
//  def reverseDependencies {
//    val k = KConfigParser.parseKConfig(
//      """
//      config A boolean
//      config B boolean {
//        prompt "..." if []
//        select A if []
//      }
//      config C boolean {
//        prompt "..." if []
//        select A if []
//      }
//      """)
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).format(System.out)
//  }
//
//  @Test
//  def multipleDefaults {
//    val k = KConfigParser.parseKConfig(
//      """
//      config A boolean {
//        default [y] if [B]
//        default [m] if [C]
//        default [n] if [D]
//      }
//      """)
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).format(System.out)
//
//  }
//
//  //TODO assertion
//  @Test
//  def defaultY {
//    val k = KConfigParser.parseKConfig(
//      """
//      config A boolean {
//        default [y] if [R]
//      }
//      config B tristate {
//       default [y] if [S]
//      }
//      """)
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).format(System.out)
//  }
//
//  @Test
//  def prompt {
//    val k = KConfigParser.parseKConfig(
//      """
//      config A boolean {
//        prompt "..." if [R]
//        default [y] if [S]
//      }
//      """)
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).format(System.out)
//  }
//
//  @Test
//  def choice {
//    val k = KConfigParser.parseKConfig(
//      """
//      choice boolean {
//          prompt "High Memory Support" if []
//          default [NOHIGHMEM] if []
//          config NOHIGHMEM boolean {
//           prompt "off" if [<choice>]
//           inherited [<choice>]
//          }
//          config HIGHMEM4G boolean {
//           prompt "4GB" if [<choice>]
//           inherited [<choice>]
//          }
//          config HIGHMEM64G boolean {
//           prompt "64GB" if [<choice>]
//           inherited [<choice>]
//          }
//         }
//      """)
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).format(System.out)
//  }
//
//  @Test
//  def nesting {
//    val k = KConfigParser.parseKConfig(
//      """
//      config A boolean {
//        config B boolean {
//          config C boolean
//        }
//      }
//      """)
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).format(System.out)
//  }
//
//  @Test
//  def tristate {
//    val k = KConfigParser.parseKConfig(
//      """
//      config A tristate
//      """)
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).format(System.out)
//  }
//
//  @Test
//  def menu {
//    val k = KConfigParser.parseKConfig(
//      """
//      menu "Hello Test" {
//        config X boolean
//        config Y boolean
//        config Z boolean
//      }
//      """)
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).format(System.out)
//
//  }
//
//
//  @Test
//  def bool1 {
//    val k = KConfigParser.parseKConfig {
//      """
//      menu "Hello World" {
//        depends on [X]
//        config A tristate {
//          prompt "..." if [B]
//          default [y] if [C]
//          choice boolean {
//            prompt "Some Choice" if []
//            config C1 boolean {
//              prompt "..." if []
//            }
//            config C2 boolean {
//              prompt "..." if []
//            }
//            config C3 boolean {
//              prompt "..." if []
//            }
//          }
//        }
//      }
//      """
//    }
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).print
//  }
//
//
//  @Test
//  def underscore {
//    val k = KConfigParser.parseKConfig {
//      """
//      config ABC_XYZ boolean {
//        prompt "..." if [A_1 || B_1]
//      }
//      """
//    }
//
//    val fm = mkFeatureModel(Hierarchy.mkHierarchyMap(k), k)
//    toText(fm).print
//  }
//}