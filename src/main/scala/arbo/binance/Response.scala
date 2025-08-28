package arbo
package binance

import data._
import kraken.{CurrencyPair, Ticker, AssetPairOptions, FeeOption}

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

import cats.data.NonEmptyList
import cats.syntax.functor._

/**
 * Binance API response models with Circe decoders.
 * 
 * This object contains all the data models needed to parse responses
 * from the Binance API, including exchange info, ticker data, and order responses.
 */
object Response {

  /**
   * Binance exchange information response.
   */
  case class ExchangeInfo(
    timezone: String,
    serverTime: Long,
    rateLimits: List[RateLimit],
    exchangeFilters: List[Json],
    symbols: List[SymbolInfo]
  )

  case class RateLimit(
    rateLimitType: String,
    interval: String,
    intervalNum: Int,
    limit: Int
  )

  case class SymbolInfo(
    symbol: String,
    status: String,
    baseAsset: String,
    baseAssetPrecision: Int,
    quoteAsset: String,
    quotePrecision: Int,
    quoteAssetPrecision: Int,
    orderTypes: List[String],
    icebergAllowed: Boolean,
    ocoAllowed: Boolean,
    isSpotTradingAllowed: Boolean,
    isMarginTradingAllowed: Boolean,
    filters: List[Json],
    permissions: List[String]
  )

  /**
   * Binance 24hr ticker statistics response.
   */
  case class Ticker24hr(
    symbol: String,
    priceChange: String,
    priceChangePercent: String,
    weightedAvgPrice: String,
    prevClosePrice: String,
    lastPrice: String,
    lastQty: String,
    bidPrice: String,
    bidQty: String,
    askPrice: String,
    askQty: String,
    openPrice: String,
    highPrice: String,
    lowPrice: String,
    volume: String,
    quoteVolume: String,
    openTime: Long,
    closeTime: Long,
    firstId: Long,
    lastId: Long,
    count: Int
  )

  /**
   * Binance order book response.
   */
  case class OrderBook(
    lastUpdateId: Long,
    bids: List[(String, String)],
    asks: List[(String, String)]
  )

  /**
   * Binance order response.
   */
  case class OrderResponse(
    symbol: String,
    orderId: Long,
    orderListId: Long,
    clientOrderId: String,
    transactTime: Long,
    price: String,
    origQty: String,
    executedQty: String,
    cummulativeQuoteQty: String,
    status: String,
    timeInForce: String,
    type: String,
    side: String,
    fills: Option[List[Fill]]
  )

  case class Fill(
    price: String,
    qty: String,
    commission: String,
    commissionAsset: String,
    tradeId: Long
  )

  /**
   * Binance account information response.
   */
  case class AccountInfo(
    makerCommission: Int,
    takerCommission: Int,
    buyerCommission: Int,
    sellerCommission: Int,
    canTrade: Boolean,
    canWithdraw: Boolean,
    canDeposit: Boolean,
    updateTime: Long,
    accountType: String,
    balances: List[Balance],
    permissions: List[String]
  )

  case class Balance(
    asset: String,
    free: String,
    locked: String
  )

  /**
   * Binance error response.
   */
  case class BinanceError(
    code: Int,
    msg: String
  )

  // Circe decoders
  implicit val rateLimitDecoder: Decoder[RateLimit] = deriveDecoder
  implicit val symbolInfoDecoder: Decoder[SymbolInfo] = deriveDecoder
  implicit val exchangeInfoDecoder: Decoder[ExchangeInfo] = deriveDecoder

  implicit val ticker24hrDecoder: Decoder[Ticker24hr] = deriveDecoder

  implicit val orderBookDecoder: Decoder[OrderBook] = deriveDecoder

  implicit val fillDecoder: Decoder[Fill] = deriveDecoder
  implicit val orderResponseDecoder: Decoder[OrderResponse] = deriveDecoder

  implicit val balanceDecoder: Decoder[Balance] = deriveDecoder
  implicit val accountInfoDecoder: Decoder[AccountInfo] = deriveDecoder

  implicit val binanceErrorDecoder: Decoder[BinanceError] = deriveDecoder

  /**
   * Convert Binance symbol to CurrencyPair.
   */
  def symbolToCurrencyPair(symbol: String): Option[CurrencyPair] = {
    // Binance symbols are typically like "BTCUSDT", "ETHBTC", etc.
    // We need to split them into base and quote currencies
    if (symbol.length >= 6) {
      // Try common quote currencies first
      val commonQuotes = List("USDT", "USDC", "BUSD", "BTC", "ETH", "BNB")
      
      commonQuotes.find { quote =>
        symbol.endsWith(quote) && symbol.length > quote.length
      }.map { quote =>
        val base = symbol.dropRight(quote.length)
        CurrencyPair(base, quote)
      }
    } else {
      None
    }
  }

  /**
   * Convert CurrencyPair to Binance symbol.
   */
  def currencyPairToSymbol(pair: CurrencyPair): String = {
    s"${pair.base}${pair.cvar}"
  }

  /**
   * Convert Binance ticker to Arbo Ticker format.
   */
  def ticker24hrToTicker(ticker: Ticker24hr): Option[Ticker] = {
    for {
      ask <- BigDecimal(ticker.askPrice).toOption
      bid <- BigDecimal(ticker.bidPrice).toOption
      close <- BigDecimal(ticker.lastPrice).toOption
      low <- BigDecimal(ticker.lowPrice).toOption
      high <- BigDecimal(ticker.highPrice).toOption
      open <- BigDecimal(ticker.openPrice).toOption
    } yield Ticker(
      ask = ask,
      bid = bid,
      closed = close,
      low = low,
      high = high,
      open = open
    )
  }

  /**
   * Convert Binance symbol info to Arbo AssetPairOptions format.
   */
  def symbolInfoToAssetPairOptions(symbolInfo: SymbolInfo): Option[AssetPairOptions] = {
    if (symbolInfo.status != "TRADING") {
      None
    } else {
      // Extract fee information from filters (this is a simplified version)
      // In a real implementation, you'd parse the filters to get actual fee rates
      val defaultTakerFee = FeeOption(0, 0.001) // 0.1% default
      val defaultMakerFee = FeeOption(0, 0.001) // 0.1% default
      
      Some(AssetPairOptions(
        pairDecimals = symbolInfo.quotePrecision,
        lotDecimals = symbolInfo.baseAssetPrecision,
        volumeCurrency = symbolInfo.quoteAsset,
        taker = NonEmptyList.one(defaultTakerFee),
        maker = List(defaultMakerFee)
      ))
    }
  }
}
