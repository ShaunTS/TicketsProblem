package test.unit.tickets

import org.specs2.mutable.Specification
import tickets.Solution


object TicketsSpec extends Specification {


    "Tickets Permutations" should {

        //   (---- 7 ---) (-- 4 --)
        val a1 = Array(2, 4, 6, 7, 8, 9, 10, 11, 12, 14, 16, 18, 20)

        //   (--- 6 ---) (--- 5 --)
        //  * Solution requires passing over an ideal 7-day ticket
        //      and choosing a combined 6-day and 5-day tickets
        val a1B = Array(2, 4, 6, 7, 8, 9, 10, 11, 12, 14, 16, 18)

        // (--- 30 ---) (--- 30 ---) (- 7(4) -) ( 2 )
        val a2 = Array.range(1, 64) ++ Array(67, 70)

        "Pair an ideal 7-day with a least ideal 7-day" in {

            Solution.solution(a1) must be_==(18)
        }

        "Skip an ideal 7, in favor of two less ideal 5 and 6-day tickets" in {

            Solution.solution(a1B) must be_==(17)
        }

        "Coorectly combine 30-day, 7-day, and 1-day ticket permutations" in {

            Solution.solution(a2) must be_==(59)
        }
    }
}