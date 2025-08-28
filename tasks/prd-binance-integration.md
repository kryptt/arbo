# Product Requirements Document: Binance Exchange Integration

## Document Information
- **Product**: Arbo Cryptocurrency Trading Bot
- **Feature**: Binance Exchange Integration with Multi-Exchange Trading Logic  
- **Version**: 1.0
- **Date**: 2024
- **Author**: AI Engineering Assistant

---

## 1. Executive Summary

This PRD outlines the integration of Binance exchange into the Arbo trading bot, currently supporting only Kraken. The integration will enable the bot to analyze trading opportunities across both exchanges and select the optimal path for maximum profit, leveraging the existing recursion scheme-based algorithm architecture.

## 2. Background & Context

### Current State
- Arbo trading bot uses sophisticated recursion schemes (Droste + Elgot morphisms) for optimal trading path discovery
- Currently integrated only with Kraken exchange via dedicated `kraken` package
- Core `Calculator` algorithm is exchange-agnostic and operates on abstract `SellOrder` types
- Clean architectural separation between exchange-specific code and core trading logic

### Business Problem
- Limited to single exchange reduces profit opportunities
- Cross-exchange arbitrage opportunities are missed
- Market depth and liquidity variations across exchanges not leveraged
- Single point of failure if Kraken API experiences issues

### Success Metrics
- **Technical**: Successful multi-exchange order execution with <5% error rate
- **Performance**: Trading decision time increase <2x current performance  
- **Profit**: 15-25% increase in profitable trading opportunities
- **Reliability**: 99.5% uptime across both exchanges

## 3. Requirements

### 3.1 Functional Requirements

#### 3.1.1 Binance Exchange Integration

**REQ-001: Binance API Client**
- Implement `BinanceRestClient` following existing `RestClient` trait interface
- Support Binance REST API v3 for:
  - Asset pair information retrieval
  - Real-time ticker data
  - Order execution
  - Account balance queries
- Implement proper authentication using Binance API key/secret
- Include rate limiting compliance:
  - REQUEST_WEIGHT: 6,000 per minute 
  - RAW_REQUESTS: 61,000 per 5 minutes
  - ORDERS: Account-specific limits (varies by interval: per second, per day, etc.)

**REQ-002: Binance Order Types**
- Create `BinanceOrder` trait extending `SellOrder`
- Implement Binance-specific order parameters:
  - Symbol notation (e.g., "BTCUSDT")
  - Order types: `LIMIT`, `MARKET`, `STOP_LOSS`, `STOP_LOSS_LIMIT`, `TAKE_PROFIT`, `TAKE_PROFIT_LIMIT`, `LIMIT_MAKER`
  - Time in force: `GTC` (Good Till Canceled), `IOC` (Immediate or Cancel), `FOK` (Fill or Kill)
  - Side: `BUY`, `SELL`
  - Quantity precision handling per symbol
  - Self-trade prevention modes: `NONE`, `EXPIRE_TAKER`, `EXPIRE_MAKER`, `EXPIRE_BOTH`
- Support mandatory parameters based on order type:
  - `LIMIT`: `timeInForce`, `quantity`, `price`
  - `MARKET`: `quantity` or `quoteOrderQty`
  - `STOP_LOSS`: `quantity`, `stopPrice` or `trailingDelta`
  - `STOP_LOSS_LIMIT`: `timeInForce`, `quantity`, `price`, `stopPrice` or `trailingDelta`

**REQ-003: Binance Data Models**
- Implement Binance-specific response models:
  - `BinanceAssetPairsInfo` 
  - `BinanceTickerResponse`
  - `BinanceSalesResponse`
  - `BinanceExecutionResponse`
- Ensure proper JSON parsing with Circe decoders
- Handle Binance-specific error response formats

#### 3.1.2 Multi-Exchange Trading Logic

**REQ-004: Exchange Abstraction Layer**
- Create abstract `Exchange` trait to unify Kraken and Binance interfaces
- Implement `ExchangeRegistry` to manage multiple exchange instances
- Add exchange identification metadata (name, fees, supported pairs)

**REQ-005: Cross-Exchange Calculator Enhancement**
- Modify `Calculator.selection` to accept multiple `GetSellOptions` sources
- Enhance recursion scheme coalgebra to consider cross-exchange paths
- Implement exchange-aware tree node generation in `SellTree`
- Add exchange routing information to `SellSeed` type

**REQ-006: Multi-Exchange Order Comparison**
- Extend `SellSelection` to include exchange source information
- Implement cross-exchange profit comparison logic
- Add exchange-specific fee calculation in profit optimization
- Handle currency pair availability differences between exchanges

**REQ-007: Optimal Exchange Selection**
- Implement exchange selection algorithm considering:
  - Net profit after fees
  - Market depth and liquidity
  - API response times
  - Historical execution success rates
- Add configurable exchange preference weights

#### 3.1.3 Configuration Management

**REQ-008: Multi-Exchange Configuration**
- Extend configuration to support multiple exchanges:
  ```scala
  case class ExchangeConfig(
    kraken: Option[KrakenConfig],
    binance: Option[BinanceConfig],
    preferences: ExchangePreferences
  )
  ```
- Support environment variable configuration for Binance:
  - `BINANCE_API_KEY`
  - `BINANCE_SECRET_KEY`
  - `BINANCE_TESTNET` (for testing)

**REQ-009: Exchange Selection Configuration**
- Add configurable parameters:
  - Maximum search depth per exchange
  - Exchange timeout settings
  - Fee tolerance thresholds
  - Minimum profit margins per exchange

### 3.2 Non-Functional Requirements

#### 3.2.1 Performance
- **Response Time**: Multi-exchange trading decisions within 5 seconds
- **Throughput**: Support 100+ concurrent trading evaluations
- **Memory**: Total memory usage increase <30% from current baseline
- **CPU**: Algorithm complexity increase <2x current performance

#### 3.2.2 Reliability
- **Availability**: 99.5% uptime considering both exchanges
- **Fault Tolerance**: Graceful degradation when one exchange is unavailable
- **Error Recovery**: Automatic retry with exponential backoff
- **Circuit Breaker**: Disable failing exchanges temporarily

#### 3.2.3 Security
- **Credential Management**: Secure storage of multiple API credentials
- **Secret Redaction**: Automatic redaction in logs for all exchange credentials
- **Network Security**: HTTPS-only communication with proper certificate validation
- **Rate Limiting**: Respect exchange-specific rate limits

#### 3.2.4 Maintainability
- **Code Organization**: Follow existing package structure patterns
- **Type Safety**: Extensive use of Scala type system for compile-time safety
- **Testing**: Comprehensive test coverage for new exchange integration
- **Documentation**: Clear API documentation and integration guides

## 4. Technical Design

### 4.1 Architecture Overview

```
src/main/scala/arbo/
├── exchanges/                    # New: Exchange abstraction layer
│   ├── Exchange.scala           # Exchange trait definition
│   ├── ExchangeRegistry.scala   # Multi-exchange management
│   └── ExchangeSelector.scala   # Exchange selection logic
├── binance/                     # New: Binance integration
│   ├── Config.scala            # Binance configuration
│   ├── RestClient.scala        # Binance API client
│   ├── Order.scala             # Binance order types
│   ├── Assets.scala            # Binance asset definitions
│   └── Response.scala          # Binance response models
├── Calculator.scala             # Enhanced: Multi-exchange support
├── Main.scala                   # Enhanced: Multi-exchange config
└── kraken/                      # Existing: No changes to interface
    └── ...
```

### 4.2 Core Changes

#### 4.2.1 Enhanced Calculator Interface
```scala
def multiExchangeSelection[M[_]: Monad](
    exchanges: NonEmptyList[Exchange[M]],
    terminalCurrency: Currency,
    maxDepth: Depth
)(holding: Holding): M[SellSelection[SellOrder]]
```

#### 4.2.2 Exchange Abstraction
```scala
trait Exchange[F[_]] {
  def name: String
  def supportedPairs: F[Set[CurrencyPair]]
  def getSellOptions(holding: Holding): F[SellOptions[SellOrder]]
  def execute(order: SellOrder): F[Holding]
  def healthCheck: F[ExchangeHealth]
}
```

#### 4.2.3 Enhanced Data Models
```scala
// Enhanced SellOrder with exchange metadata
case class ExchangeOrder(
  exchange: String,
  underlying: SellOrder
) extends SellOrder

// Enhanced SellSelection with exchange routing
case class MultiExchangePath(
  orders: NonEmptyList[ExchangeOrder],
  totalProfit: BigDecimal,
  exchangeRoute: List[String]
) extends SellSelection[ExchangeOrder]
```

### 4.3 API Integration Specifications

#### 4.3.1 Binance REST API Integration
- **Base URLs**: 
  - Primary: `https://api.binance.com`
  - Alternatives: `https://api-gcp.binance.com`, `https://api1.binance.com` through `https://api4.binance.com`
  - Public data only: `https://data-api.binance.vision`
- **Authentication**: 
  - API keys via `X-MBX-APIKEY` header
  - HMAC SHA256 signature for TRADE and USER_DATA endpoints
  - `timestamp` parameter required (milliseconds)
  - `recvWindow` parameter (max 60,000ms)
- **Rate Limits**: 
  - REQUEST_WEIGHT: 6,000 per minute (weight-based)
  - RAW_REQUESTS: 61,000 per 5 minutes  
  - ORDERS: Per-account limits (varies by interval)
  - IP-based tracking via `X-MBX-USED-WEIGHT-*` headers
- **Key Endpoints**:
  - `/api/v3/exchangeInfo` - Exchange trading rules and symbol information (Weight: 20)
  - `/api/v3/ticker/24hr` - 24hr rolling window price change statistics (Weight: 2-80)
  - `/api/v3/order` - Place new order (Weight: 1)
  - `/api/v3/account` - Account information (Weight: 20)
  - `/api/v3/depth` - Order book depth (Weight: 2-100 based on limit)

#### 4.3.2 Error Handling Strategy
- **Network Errors**: Exponential backoff retry (1s, 2s, 4s, 8s)
- **API Errors**: Exchange-specific error parsing and handling
- **Rate Limiting**: 
  - HTTP 429: Rate limit exceeded - back off immediately 
  - HTTP 418: IP banned - respect `Retry-After` header (2 minutes to 3 days)
  - Monitor `X-MBX-USED-WEIGHT-*` headers for proactive throttling
  - Queue-based request throttling with weight calculation
- **API Timeouts**: 10-second timeout with status unknown handling
- **Failover**: Automatic switch to available exchanges with circuit breaker pattern

### 4.4 Security Considerations

#### 4.4.1 Credential Management
```scala
case class BinanceConfig(
  apiKey: String,
  secretKey: ByteVector,
  testnet: Boolean = false
)

object BinanceConfig {
  implicit val show = Show.show[BinanceConfig] { cfg =>
    s"BinanceConfig(${cfg.apiKey.take(6)}..., testnet=${cfg.testnet})"
  }
}
```

#### 4.4.2 Secure Communication
- All API calls over HTTPS with certificate pinning
- Request signing using HMAC-SHA256
- Nonce/timestamp inclusion for replay attack prevention
- Sensitive data redaction in all log outputs

## 5. Implementation Plan

### 5.1 Phase 1: Foundation (Weeks 1-2)
- **Sprint 1.1**: Create exchange abstraction layer
  - Implement `Exchange` trait
  - Create `ExchangeRegistry`
  - Add exchange health monitoring
- **Sprint 1.2**: Binance API client foundation
  - Implement basic `BinanceRestClient`
  - Add authentication and security
  - Create Binance data models

### 5.2 Phase 2: Core Integration (Weeks 3-4)
- **Sprint 2.1**: Binance order types and execution
  - Implement `BinanceOrder` variants
  - Add order execution logic
  - Implement proper error handling
- **Sprint 2.2**: Multi-exchange calculator enhancement
  - Modify `Calculator` for multi-exchange support
  - Enhance recursion scheme for cross-exchange paths
  - Add exchange selection logic

### 5.3 Phase 3: Integration & Testing (Weeks 5-6)
- **Sprint 3.1**: Configuration and deployment
  - Multi-exchange configuration management
  - Update `Main.scala` for multi-exchange support
  - Add environment-based exchange enablement
- **Sprint 3.2**: Testing and optimization
  - Comprehensive test suite
  - Performance optimization
  - Documentation and deployment

### 5.4 Phase 4: Validation (Week 7)
- **Sprint 4.1**: End-to-end testing
  - Integration testing with both exchanges
  - Performance validation
  - Security audit

## 6. Testing Strategy

### 6.1 Unit Testing
- **Exchange Abstraction**: Test exchange registry and selection logic
- **Binance Integration**: Mock API responses for unit testing
- **Calculator Enhancement**: Test multi-exchange path generation
- **Security**: Test credential handling and redaction

### 6.2 Integration Testing
- **API Integration**: Test against Binance testnet API
- **Multi-Exchange Flows**: Test cross-exchange trading scenarios
- **Error Handling**: Test failover and retry mechanisms
- **Performance**: Load testing with concurrent requests

### 6.3 Test Structure
```
src/test/scala/arbo/
├── exchanges/
│   ├── ExchangeRegistrySpec.scala
│   └── ExchangeSelectorSpec.scala
├── binance/
│   ├── RestClientSpec.scala
│   ├── OrderSpec.scala
│   └── ConfigSpec.scala
├── CalculatorMultiExchangeSpec.scala
└── integration/
    └── MultiExchangeIntegrationSpec.scala
```

## 7. Risk Analysis

### 7.1 Technical Risks

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Binance API rate limiting | High | Medium | Implement intelligent rate limiting and caching |
| Cross-exchange latency | Medium | High | Add configurable timeouts and parallel processing |
| Algorithm complexity increase | Medium | Medium | Performance monitoring and optimization |
| Exchange-specific errors | High | Medium | Robust error handling and fallback mechanisms |

### 7.2 Business Risks

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Binance API changes | Medium | Low | Version pinning and API change monitoring |
| Regulatory changes | High | Low | Configurable exchange enablement |
| Exchange downtime | Medium | Medium | Multi-exchange redundancy |
| Increased operational complexity | Medium | High | Comprehensive monitoring and alerting |

## 8. Success Criteria

### 8.1 Technical Success Criteria
- [ ] Successful integration with Binance API (100% basic functionality)
- [ ] Multi-exchange trading path generation working correctly
- [ ] Performance degradation <2x current baseline
- [ ] Comprehensive test coverage >85%
- [ ] Zero security vulnerabilities in security audit

### 8.2 Business Success Criteria
- [ ] 15-25% increase in profitable trading opportunities
- [ ] Cross-exchange arbitrage opportunities identified and executed
- [ ] System reliability maintained at 99.5% uptime
- [ ] Successful production deployment with zero critical issues

## 9. Dependencies & Assumptions

### 9.1 External Dependencies
- Binance API availability and stability
- Continued support for Binance REST API v3
- Network connectivity between application and both exchanges
- Sufficient API rate limits for trading volume

### 9.2 Technical Assumptions
- Current recursion scheme algorithm can be efficiently extended for multi-exchange
- Scala 2.13.1 ecosystem supports Binance integration requirements
- Current infrastructure can handle increased computational load
- Exchange fee structures remain relatively stable

### 9.3 Business Assumptions
- Multi-exchange trading provides significant competitive advantage
- Increased complexity is justified by profit improvements
- Team has sufficient expertise for maintenance and operations
- Regulatory environment supports multi-exchange trading

## 10. Post-Launch Monitoring

### 10.1 Key Metrics
- **Trading Performance**: Profit increase percentage
- **System Performance**: Response times, error rates, throughput
- **Exchange Health**: API success rates, latency metrics
- **Security**: Authentication failures, suspicious activity

### 10.2 Monitoring Tools
- Application performance monitoring (APM)
- Exchange API health dashboards
- Trading profit analytics
- Security event logging and alerting

### 10.3 Maintenance Plan
- Weekly exchange API health reviews
- Monthly performance optimization reviews
- Quarterly security audits
- Annual architecture review for scalability

---

## Appendix A: API Endpoint Specifications

### Binance REST API Endpoints

#### Asset Information
```
GET /api/v3/exchangeInfo
Weight: 20
Security: NONE
Parameters: 
  - symbol (STRING, optional): Example: BNBBTC
  - symbols (ARRAY, optional): Example: ["BNBBTC","BTCUSDT"]  
  - permissions (ENUM, optional): SPOT, MARGIN, LEVERAGED
  - showPermissionSets (BOOLEAN, optional): Default true
  - symbolStatus (ENUM, optional): TRADING, HALT, BREAK
Response: Exchange trading rules and symbol information
```

#### Ticker Information  
```
GET /api/v3/ticker/24hr
Weight: 2 for single symbol, 4-80 for multiple symbols
Security: NONE
Parameters:
  - symbol (STRING, optional): Single trading pair
  - symbols (ARRAY, optional): Multiple trading pairs
  - type (ENUM, optional): FULL or MINI
Response: 24hr rolling window price change statistics
```

#### Order Book Depth
```
GET /api/v3/depth  
Weight: 2-100 (based on limit parameter)
Security: NONE
Parameters:
  - symbol (STRING, required): Trading pair
  - limit (INT, optional): Default 100, Max 5000
Response: Order book depth data
```

#### Order Placement
```
POST /api/v3/order
Weight: 1
Unfilled Order Count: 1
Security: TRADE (HMAC SHA256 signature required)
Parameters: 
  - symbol (STRING, required): Trading pair
  - side (ENUM, required): BUY or SELL
  - type (ENUM, required): Order type
  - timeInForce (ENUM, conditional): GTC, IOC, FOK
  - quantity (DECIMAL, conditional)
  - quoteOrderQty (DECIMAL, conditional)  
  - price (DECIMAL, conditional)
  - stopPrice (DECIMAL, conditional)
  - newClientOrderId (STRING, optional)
  - recvWindow (LONG, optional): Max 60000
  - timestamp (LONG, required): Milliseconds
Response: Order execution details
```

#### Account Information
```
GET /api/v3/account
Weight: 20
Security: USER_DATA (HMAC SHA256 signature required)
Parameters:
  - recvWindow (LONG, optional): Max 60000
  - timestamp (LONG, required): Milliseconds
Response: Current account information including balances
```

#### Order Rate Limit Status
```
GET /api/v3/rateLimit/order
Weight: 40
Security: USER_DATA (HMAC SHA256 signature required)
Parameters:
  - recvWindow (LONG, optional): Max 60000
  - timestamp (LONG, required): Milliseconds
Response: Current unfilled order count for all intervals
```

## Appendix B: Configuration Examples

### Environment Variables
```bash
# Existing Kraken configuration
export API_KEY="kraken_api_key"
export PRIVATE_KEY="base64_encoded_kraken_private_key"

# New Binance configuration
export BINANCE_API_KEY="binance_api_key"  
export BINANCE_SECRET_KEY="base64_encoded_binance_secret"
export BINANCE_TESTNET="false"

# Multi-exchange preferences
export EXCHANGE_TIMEOUT_MS="5000"
export MAX_DEPTH_PER_EXCHANGE="3"
export PREFERRED_EXCHANGES="kraken,binance"
```

### Application Configuration
```scala
case class MultiExchangeConfig(
  kraken: Option[Secret[KrakenConfig]],
  binance: Option[Secret[BinanceConfig]], 
  preferences: ExchangePreferences
)

case class ExchangePreferences(
  timeoutMs: FiniteDuration,
  maxDepthPerExchange: Int,
  preferredOrder: List[String],
  minProfitThreshold: BigDecimal
)
```

---

*This PRD follows enterprise software development standards and provides comprehensive guidance for implementing Binance exchange integration with multi-exchange trading optimization in the Arbo cryptocurrency trading bot.*
