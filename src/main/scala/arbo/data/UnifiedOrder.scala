package arbo
package data

/**
 * Unified order type that can represent orders from any exchange.
 * 
 * This provides a common interface for orders across different exchanges
 * while maintaining exchange-specific information.
 */
sealed trait UnifiedOrder extends SellOrder {
  def exchangeName: String
  def exchangeSpecificData: Map[String, String]
}

object UnifiedOrder {
  
  /**
   * Create a unified order from a Kraken order.
   */
  def fromKrakenOrder(krakenOrder: kraken.KrakenOrder): UnifiedOrder = {
    new UnifiedOrder {
      def from: Currency = krakenOrder.from
      def to: Currency = krakenOrder.to
      def price: Price = krakenOrder.price
      def fromAmmount: Ammount = krakenOrder.fromAmmount
      def fee: Fee = krakenOrder.fee
      def exchangeName: String = "kraken"
      def exchangeSpecificData: Map[String, String] = Map(
        "krakenPair" -> krakenOrder.krakenPair,
        "krakenPrice" -> krakenOrder.krakenPrice,
        "krakenVolume" -> krakenOrder.krakenVolume,
        "krakenType" -> krakenOrder.krakenType
      )
    }
  }
  
  /**
   * Create a unified order from a Binance order.
   */
  def fromBinanceOrder(binanceOrder: binance.BinanceOrder): UnifiedOrder = {
    new UnifiedOrder {
      def from: Currency = binanceOrder.from
      def to: Currency = binanceOrder.to
      def price: Price = binanceOrder.price
      def fromAmmount: Ammount = binanceOrder.fromAmmount
      def fee: Fee = binanceOrder.fee
      def exchangeName: String = "binance"
      def exchangeSpecificData: Map[String, String] = Map(
        "orderType" -> binanceOrder.getClass.getSimpleName
      )
    }
  }
  
  /**
   * Convert back to exchange-specific order types.
   */
  def toKrakenOrder(unifiedOrder: UnifiedOrder): Option[kraken.KrakenOrder] = {
    if (unifiedOrder.exchangeName == "kraken") {
      // This would need to be implemented based on the specific Kraken order type
      // For now, return None as we'd need to reconstruct the specific order type
      None
    } else {
      None
    }
  }
  
  def toBinanceOrder(unifiedOrder: UnifiedOrder): Option[binance.BinanceOrder] = {
    if (unifiedOrder.exchangeName == "binance") {
      // This would need to be implemented based on the specific Binance order type
      // For now, return None as we'd need to reconstruct the specific order type
      None
    } else {
      None
    }
  }
}
