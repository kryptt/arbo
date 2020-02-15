package arbo

package object kraken {

  type FeesResponse = Map[CurrencyPair, FeeOptions]

  type TickerResponse = Map[CurrencyPair, Ticker]

}
