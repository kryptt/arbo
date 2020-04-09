package arbo

import io.circe.Json

package object kraken {

  type AssetPairsInfo = Map[CurrencyPair, AssetPairOptions]

  type TickerResponse = Map[CurrencyPair, Ticker]

  type SalesResponse = List[SaleLine]

  type Order = Json

}
