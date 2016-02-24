package tickets

import java.util.{BitSet}
import scala.annotation.tailrec
import scala.collection.mutable.{MutableList => MutaList}

package object ImplicitArrayConversion {

    implicit class SmartArray[T](xs: Array[T]) {

        def get(index: Int): Option[T] = {

            Some(index).filter(xs.isDefinedAt).map(xs)
        }
    }
}

package object helpers {

    import ImplicitArrayConversion._

    type TicketTyp = (Int, Int, Int, Int)

    /** Accumulates a set of non-overlapping ticket intervals by adding subsequent
     *  selections to a mutable list, before using that list to check the next potential
     *  selection for overlaps.
     *
     *  @param tickets The full list of ticket permutations, zipped with index.
     *  @param A mutable list that will accumulate consequtive spanning ticket selections.
     */
    @tailrec
    def nextTickets(tickets: Array[(TicketTyp, Int)], blocked: MutaList[(TicketTyp, Int)]): Unit = {

        val ticketFound = tickets.find { case(ticketA, _) =>

            !blocked.exists { case(ticketB, _) => isOverlap(ticketB, ticketA) }

        }.map { ticket => blocked +=(ticket); ticket }.isDefined

        if(!ticketFound) return

        nextTickets(tickets, blocked)
    }

    /** Generates every possible permutation of subsequent array elements, in groups of
     *  `groupSize`, filters out groups whose value differance exceeds the max dayRange, finally
     *  any groups whose dayRange would stretch over into subsequent array elements are filtered out.
     *
     *  @param A The initial input array.
     *  @param groupSize The number of subsequent array elements to group
     *  @param dayRange The max alowable difference between the first and last
     *      elements of each group
     *
     *  @return The total list of ticket permutations for the given group size.
     */
    def groupedByRange(A: Array[Int], groupSize: Int, dayRange: Int): Array[TicketTyp] = {
        val interval = groupSize - 1
        val maxRange = dayRange - 1

        val cost: Int = maxRange match {
            case 6 => 7
            case _ => 25
        }

        Array.range(0, A.length - interval).map(i => (i, i + interval)).map {
            case(from, to) => (from, to, (to-from)+1, A(to) - A(from), A.get(to+1))
        }.filterNot {
            case(_, _, _, diff, _) => diff > maxRange
        }.filter {
            case(from, to, days, _, Some(next)) => (next - A(from) > maxRange)
            case(_, _, _, _, None) => true
        }.map {
            case(from, to, days, _, _) => (from, to, days, cost)
        }
    }

    def isOverlap(a: (Int, Int), b: (Int, Int)): Boolean = {
        val (a0, a1) = a
        val (b0, b1) = b

        if( (a0 <= b0 && b0 <= a1) || (a0 <= b1 && b1 <= a1) ||
            (b0 <= a0 && a0 <= b1) || (b0 <= a1 && a1 <= b1)
        ) true

        else false
    }

    def isOverlap(a: TicketTyp, b: TicketTyp): Boolean = {
        val (fromA, toA, _, _) = a
        val (fromB, toB, _, _) = b

        isOverlap( (fromA, toA), (fromB, toB) )
    }

    def calculateCost(arr: Array[Int], bought: MutaList[(TicketTyp, Int)]): Long = {

        val daysCovered = bought.map {
            case ((_, _, days, _), _) => days
        }.sum.toLong

        val totalPrice = bought.map {
            case ((_, _, _, price), _) => price
        }.sum

        (arr.length - daysCovered)*2 + totalPrice
    }
}

import helpers._

object TicketPermutations {

    def generate(arr: Array[Int]): Array[(TicketTyp, Int)] = {
        (thirtyDayGroups(arr) ++ sevenDayGroups(arr)).zipWithIndex
    }

    def sevenDayGroups(arr: Array[Int]): Array[TicketTyp] = {

        Array(7, 6, 5, 4).map(groupedByRange(arr, _, 7)).fold(Array.empty[TicketTyp]) {
            case (a, b) => a ++ b
        }
    }

    def thirtyDayGroups(arr: Array[Int]): Array[TicketTyp] = {

        Array.range(15, 31).reverse.map(groupedByRange(arr, _, 30))
            .fold(Array.empty[TicketTyp]) {
                case (a, b) => a ++ b
            }
    }
}

object Solution {

    def solution(A: Array[Int]): Int = {

        val tickets: Array[(TicketTyp, Int)] = TicketPermutations.generate(A)

        val tested: BitSet = new BitSet()

        var lowest: Long = A.length*2

        while(tested.nextClearBit(0) < tickets.length) {

            val selected: MutaList[(TicketTyp, Int)] = MutaList(
                tickets(tested nextClearBit 0)
            )

            nextTickets(tickets, selected)

            selected.foreach {
                case (ticket, index) => tested.set(index)
            }

            // // Uncomment to view trace of all ticket combinations tested
            // println("________________________________________")
            // selected.foreach(println)
            // println("------------------------------------------")


            lowest = scala.math.min(lowest, calculateCost(A, selected))
        }

        lowest.toInt
    }
}