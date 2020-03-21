package arbo

import io.circe.Json

package object kraken {

  type FeesResponse = Map[CurrencyPair, FeeOptions]

  type TickerResponse = Map[CurrencyPair, Ticker]

  type SalesResponse = List[SaleLine]

  type Order = Json

}
