import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class prova extends AggregateProgram with FieldUtils with StandardSensors with ScafiAlchemistSupport {

  def count() = {
    rep(0) { n => n + 1 }
  } // Conteggio dei round
  def constant[T](value: => T) = {
    rep(value) { old => old }
  } // Inizializzazione di valori all'inizio di ogni Round
  def gradient(isSource: Boolean, metric: => Double) = {
    rep(Double.PositiveInfinity) { distance =>
      mux(isSource){ 0.0 }{ minHood{ nbr{distance} + metric } }
    }
  }  // Funzione per il calcolo della stima della distanza

  def numNeighbors() = {
    sumHood(nbr{1})
  }                        // Esercizio 1: Calcolare numero dei vicini
  def maxNeighbors() = {
    rep(0){x => max(x, numNeighbors())}
  }                        // Esercizio 2: Calcolare il massimo numero dei vicini avuti dal corrente device
  def maxNeighborsAll() = {
    rep(0){x => max(x, maxHood(nbr{maxNeighbors()}))}
  }                     // Esercizio 3: Calcolare il massimo numero dei vicini mai avuti da un device della rete
  def minNeighborsCoordinates() = {
    snd(minHood(nbr{pair(numNeighbors(), getCoordinates())}))
  }    // Esercizio 4: Calcolare le coordinate del device con il minimo numero di vicini
  def maxNeighborsCoordinates() = {
    snd(maxHood(nbr{pair(numNeighbors(), getCoordinates())}))
  }    // Esercizio 5: Calcolare le coordinate del device con il massimo numero di vicini

  override def main() = {
    node.put("language", "scafi")
    val sourceID = 0  // Nodo 0: Sorgente del campo
    val isSource = mid == sourceID

    node.put(
      "distance",
      alchemistEnvironment.getDistanceBetweenNodes(
        alchemistEnvironment.getNodeByID(mid),
        alchemistEnvironment.getNodeByID(sourceID))
    ) // Calcolo della distanza
    node.put(
      "gradient",
      gradient(isSource, nbrRange)
    ) // Calcolo della stima della distanza

    val timeToGo = constant(10 * nextRandom())
    node.put(
      "timeToGo",
      timeToGo
    ) // Tempo di inizio del movimento

    branch(timestamp() < timeToGo){ } {
      val d = getCoordinates()
      val m = minNeighborsCoordinates()
      val M = maxNeighborsCoordinates()

      val x = (d.head + m.head)/2
      val y = (d(1) + m(1))/2
      node.put("target", List(x, y))
    } // Esercizio 6: Effettuare un movimento del device verso device con minimo numero di vicini e lontano dal device con il massimo numero di vicini

    node.put("neighbors", numNeighbors())
    node.put("maxNeighbors", maxNeighbors())
    node.put("maxNeighborsAll", maxNeighborsAll())
    node.put("minNeigh", minNeighborsCoordinates())
    node.put("maxNeigh", maxNeighborsCoordinates())
  }

  def pair[A,B](x : A, y : B) = {
    Tuple2(x,y)
  }                     // Crea una coppia
  def fst[A,B](t : (A, B)) = {
    t._1
  }                             // Primo elemento di una coppia
  def snd[A,B](t : (A, B)) = {
    t._2
  }                             // Secondo elemento di una coppia
  def max[T](x : T, y : T)(implicit ord: Ordering[T]) = {
    ord.max(x, y)
  }  // Massimo tra due elementi
  def sumHood[T](x : => T)(implicit numEv: Numeric[T]) = {
    includingSelf.sumHood(x)
  } // Somma di un campo di numeri
  def getCoordinates() = {
    alchemistEnvironment.getPosition(
      alchemistEnvironment.getNodeByID(mid)
    ).getCartesianCoordinates.toList
  }                      // Coordinate correnti
}
