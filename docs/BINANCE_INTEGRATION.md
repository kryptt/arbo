# Binance Integration Documentation

## Overview

This document provides comprehensive guidance for the Binance integration in the Arbo cryptocurrency trading bot. The integration enables multi-exchange trading by supporting both Kraken and Binance exchanges simultaneously.

## Architecture

### Exchange Abstraction Layer

The integration introduces a unified `Exchange[F[_], O <: SellOrder]` trait that both Kraken and Binance implementations extend:

```scala
trait Exchange[F[_], O <: SellOrder] {
  def name: String
  def supportedPairs: F[List[CurrencyPair]]
  def getSellOptions(holding: Holding): F[SellOptions[O]]
  def execute(order: O): F[Holding]
  def healthCheck: F[ExchangeHealth]
  def ticker(pairs: NonEmptyList[CurrencyPair]): F[TickerResponse]
  def assetPairs: F[AssetPairsInfo]
}
```

### Multi-Exchange Management

- **ExchangeRegistry**: Manages multiple exchange instances with health monitoring
- **ExchangeSelector**: Provides sophisticated selection logic based on profit, fees, latency, and reliability
- **CircuitBreaker**: Automatically disables unhealthy exchanges and re-enables them after recovery

### Enhanced Calculator

The Calculator now supports multi-exchange trading paths through:
- `ExchangeOrder[O]`: Wraps orders with exchange metadata
- `MultiExchangePath[O]`: Tracks profit and fees across exchanges
- `multiExchangeSelection`: Finds optimal paths across multiple exchanges

## Configuration

### Environment Variables

#### Binance Configuration
```bash
export BINANCE_API_KEY="your_binance_api_key"
export BINANCE_SECRET_KEY="base64_encoded_secret_key"
export BINANCE_TESTNET="true"  # Optional, defaults to false
```

#### Exchange Preferences
```bash
export EXCHANGE_TIMEOUT_MS="30000"  # 30 seconds
export MAX_DEPTH_PER_EXCHANGE="6"
export PREFERRED_EXCHANGES="kraken,binance"
export MIN_PROFIT_THRESHOLD="0.001"  # 0.1%
export EXCHANGE_SELECTION_WEIGHTS="profit:0.4,fee:0.3,latency:0.2,reliability:0.1"
```

#### Legacy Kraken Configuration (for backward compatibility)
```bash
export API_KEY="your_kraken_api_key"
export PRIVATE_KEY="base64_encoded_private_key"
```

### Configuration Loading

The application automatically loads configuration from environment variables:

```scala
val config = ExchangeConfig.load[IO].value
```

## API Integration

### Binance API Endpoints

The integration supports the following Binance API endpoints:

- `/api/v3/ping` - Health check
- `/api/v3/time` - Server time
- `/api/v3/exchangeInfo` - Exchange information
- `/api/v3/ticker/24hr` - 24hr ticker statistics
- `/api/v3/depth` - Order book
- `/api/v3/order` - Order management
- `/api/v3/account` - Account information

### Rate Limiting

Binance API rate limits are respected:
- **REQUEST_WEIGHT**: 6000 requests per minute
- **RAW_REQUESTS**: 61000 requests per 5 minutes

### Authentication

Binance uses HMAC SHA256 signatures for authentication:

```scala
def sign(queryString: String, secretKey: ByteVector): String = {
  val mac = Mac.getInstance("HmacSHA256")
  val secretKeySpec = new SecretKeySpec(secretKey.toArray, "HmacSHA256")
  mac.init(secretKeySpec)
  val signature = mac.doFinal(queryString.getBytes(StandardCharsets.UTF_8))
  ByteVector(signature).toBase64
}
```

### Error Handling

The integration includes comprehensive error handling:

- **HTTP 429**: Rate limit exceeded with retry-after header support
- **HTTP 418**: IP banned with retry-after header support
- **HTTP 400**: Bad request with specific error code parsing
- **Exponential backoff**: 1s, 2s, 4s, 8s retry intervals

## Order Types

Binance supports various order types:

- **MarketOrder**: Executes immediately at current market price
- **LimitOrder**: Executes only at specified price or better
- **StopLossOrder**: Triggers when price reaches stop price
- **StopLossLimitOrder**: Combines stop loss with limit order
- **TakeProfitOrder**: Triggers when price reaches take profit price
- **TakeProfitLimitOrder**: Combines take profit with limit order
- **LimitMakerOrder**: Only executes if it would be a maker order

## Multi-Exchange Trading

### Exchange Selection

The system automatically selects the best exchange based on:

1. **Profit Potential** (40% weight): Calculated from available sell options
2. **Fee Structure** (30% weight): Lower fees receive higher scores
3. **Response Latency** (20% weight): Lower latency receives higher scores
4. **Reliability** (10% weight): Based on health status and error rates

### Cross-Exchange Paths

The Calculator can find optimal trading paths that span multiple exchanges:

```scala
val result = Calculator.multiExchangeSelection(
  exchanges = NonEmptyList.of(krakenExchange, binanceExchange),
  terminalCurrency = "EUR",
  maxDepth = 6
)(holding)
```

### Health Monitoring

Each exchange is continuously monitored for:
- Response latency
- Error rates
- Success rates
- Circuit breaker status

## Security Considerations

### Credential Management

- All API keys and secrets are stored as `Secret` types
- Credentials are automatically redacted in logs
- Base64 encoding is used for private keys
- Environment variables are preferred over configuration files

### HMAC Signature Validation

- All authenticated requests include proper HMAC SHA256 signatures
- Timestamps and recvWindow parameters prevent replay attacks
- Signature validation is performed on all responses

## Performance Considerations

### Memory Usage

- Exchange registry uses efficient in-memory storage
- Circuit breaker metrics are bounded to prevent memory leaks
- Tree traversal depth is limited to prevent stack overflow

### CPU Usage

- Recursion schemes are optimized for performance
- Exchange selection uses efficient scoring algorithms
- Health checks are performed asynchronously

## Monitoring and Observability

### Health Checks

```scala
val health = exchange.healthCheck
// Returns ExchangeHealth with status, latency, errorRate, lastChecked
```

### Metrics

The system tracks:
- Exchange response times
- Error rates per exchange
- Circuit breaker states
- Trading path success rates
- Profit/loss across exchanges

### Logging

All sensitive data is automatically redacted:
- API keys show only first 6 characters
- Private keys are never logged
- Error messages exclude sensitive information

## Testing

### Unit Tests

- Exchange abstraction layer tests with ScalaCheck properties
- Order validation tests for all order types
- Configuration loading tests
- Security tests for credential handling

### Integration Tests

- Mocked API responses for all endpoints
- End-to-end tests against Binance testnet
- Multi-exchange path optimization tests
- Error handling and retry logic tests

### Performance Tests

- Memory usage validation (<30% increase from baseline)
- Response time validation (<2x performance impact)
- Load testing with multiple concurrent requests

## Deployment

### Prerequisites

- JDK 8 or higher
- SBT 1.3.9
- Environment variables configured

### Build and Run

```bash
# Compile the project
sbt compile

# Run tests
sbt test

# Run the application
sbt run

# Package for deployment
sbt universal:packageBin
```

### Environment Setup

1. Set up Binance API credentials
2. Configure exchange preferences
3. Set up monitoring and logging
4. Test against Binance testnet first

## Troubleshooting

### Common Issues

1. **Authentication Failures**
   - Verify API key format
   - Check secret key encoding
   - Ensure proper timestamp synchronization

2. **Rate Limiting**
   - Monitor request weights
   - Implement proper backoff strategies
   - Consider request batching

3. **Circuit Breaker Activation**
   - Check exchange health status
   - Review error rates and latency
   - Verify network connectivity

4. **Configuration Issues**
   - Validate environment variables
   - Check configuration parsing
   - Verify exchange preferences

### Debug Mode

Enable debug logging to troubleshoot issues:

```bash
export LOG_LEVEL="DEBUG"
```

## Future Enhancements

- Support for additional exchanges (Coinbase, Binance.US, etc.)
- Advanced order types (OCO, trailing stops)
- Real-time market data streaming
- Machine learning-based exchange selection
- Advanced risk management features

## Support

For issues and questions:
- Check the logs for error details
- Verify configuration settings
- Test with Binance testnet first
- Review the API documentation

---

*This documentation is part of the Arbo cryptocurrency trading bot implementation.*
