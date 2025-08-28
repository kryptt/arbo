## Relevant Files
- `src/main/scala/arbo/exchanges/Exchange.scala` - Abstract trait defining unified exchange interface for both Kraken and Binance
- `src/main/scala/arbo/exchanges/ExchangeHealth.scala` - Exchange health status data model for monitoring
- `src/main/scala/arbo/exchanges/ExchangeRegistry.scala` - Multi-exchange management with health monitoring and registration logic
- `src/main/scala/arbo/exchanges/ExchangeSelector.scala` - Exchange selection criteria and scoring logic
- `src/main/scala/arbo/exchanges/CircuitBreaker.scala` - Circuit breaker pattern for automatic exchange disabling/re-enabling

- `src/main/scala/arbo/exchanges/Exchange.scala` - Abstract trait defining unified exchange interface for both Kraken and Binance
- `src/main/scala/arbo/exchanges/ExchangeRegistry.scala` - Multi-exchange management with health monitoring and registration logic  
- `src/main/scala/arbo/exchanges/ExchangeHealth.scala` - Exchange health status data model for monitoring
- `src/main/scala/arbo/exchanges/ExchangeRegistry.scala` - Multi-exchange management with health monitoring and registration logic
- `src/main/scala/arbo/exchanges/ExchangeSelector.scala` - Exchange selection criteria and scoring logic
- `src/main/scala/arbo/exchanges/CircuitBreaker.scala` - Circuit breaker pattern for automatic exchange disabling/re-enabling
- `src/main/scala/arbo/binance/Config.scala` - Binance configuration with API key/secret and Show instance for redaction
- `src/main/scala/arbo/binance/RestClient.scala` - Binance API client following existing RestClient trait pattern
- `src/main/scala/arbo/binance/Order.scala` - Binance order types extending SellOrder with LIMIT, MARKET, STOP_LOSS variants
- `src/main/scala/arbo/binance/Assets.scala` - Binance asset definitions and currency pair mappings
- `src/main/scala/arbo/binance/Response.scala` - Binance API response models with Circe decoders
- `src/main/scala/arbo/binance/Security.scala` - HMAC SHA256 signature implementation for Binance authentication
- `src/main/scala/arbo/data/ExchangeOrder.scala` - Enhanced SellOrder wrapper with exchange metadata
- `src/main/scala/arbo/data/MultiExchangePath.scala` - Enhanced SellSelection with exchange routing information
- `src/main/scala/arbo/Calculator.scala` - Enhanced with multiExchangeSelection method and cross-exchange support
- `src/main/scala/arbo/Main.scala` - Updated to support multi-exchange configuration loading
- `src/test/scala/arbo/exchanges/ExchangeRegistrySpec.scala` - Tests for multi-exchange management logic
- `src/test/scala/arbo/binance/RestClientSpec.scala` - Tests for Binance API client with mocked responses
- `src/test/scala/arbo/binance/OrderSpec.scala` - Property-based tests for Binance order types
- `src/test/scala/arbo/binance/ConfigSpec.scala` - Tests for Binance configuration and credential redaction
- `src/test/scala/arbo/CalculatorMultiExchangeSpec.scala` - Tests for enhanced Calculator with multi-exchange paths
- `src/test/scala/arbo/integration/MultiExchangeIntegrationSpec.scala` - End-to-end integration tests with testnet APIs

### Notes

- Tests follow Arbo's structure: `src/test/scala/arbo/` mirrors `src/main/scala/arbo/`
- Use `sbt test` to run all tests, `sbt "testOnly arbo.SpecName"` for specific test suites  
- Property-based tests with ScalaCheck for algorithmic verification of multi-exchange logic
- Mock API calls in tests - never hit real endpoints, use testnet for integration tests only
- Maintain functional purity throughout implementation using Cats Effect IO monad
- Follow existing patterns: RestClient trait, Secret types for credentials, recursion schemes for algorithms

## Tasks

- [x] 1.0 Create Exchange Abstraction Layer
  - [x] 1.1 Create `Exchange[F[_]]` trait with name, supportedPairs, getSellOptions, execute, healthCheck methods
  - [x] 1.2 Implement `ExchangeHealth` data model with status, latency, errorRate, lastChecked fields
  - [x] 1.3 Create `ExchangeRegistry[F[_]]` for managing multiple exchange instances with registration and health monitoring
  - [x] 1.4 Implement exchange selection logic considering profit, fees, response times, and success rates
  - [x] 1.5 Add circuit breaker pattern for automatic exchange disabling/re-enabling based on health metrics

- [ ] 2.0 Implement Binance Exchange Integration
  - [ ] 2.1 Create `BinanceConfig` case class with apiKey, secretKey, testnet fields and Show instance for credential redaction
  - [ ] 2.2 Implement Binance HMAC SHA256 signature authentication with timestamp and recvWindow handling
  - [ ] 2.3 Create Binance data models: BinanceAssetPairsInfo, BinanceTickerResponse, BinanceSalesResponse, BinanceExecutionResponse with Circe decoders
  - [ ] 2.4 Implement `BinanceRestClient` following RestClient trait with rate limiting (REQUEST_WEIGHT: 6000/min, RAW_REQUESTS: 61000/5min)
  - [ ] 2.5 Add core Binance API endpoints: /exchangeInfo, /ticker/24hr, /depth, /order, /account with proper weights and error handling
  - [ ] 2.6 Create `BinanceOrder` trait extending SellOrder with LIMIT, MARKET, STOP_LOSS, STOP_LOSS_LIMIT, TAKE_PROFIT, TAKE_PROFIT_LIMIT, LIMIT_MAKER variants
  - [ ] 2.7 Implement order parameter validation with mandatory parameters per order type and quantity precision handling
  - [ ] 2.8 Add Binance error handling with HTTP 429/418 responses, Retry-After headers, exponential backoff (1s,2s,4s,8s)

- [ ] 3.0 Enhance Calculator for Multi-Exchange Support
  - [ ] 3.1 Create `ExchangeOrder` wrapper extending SellOrder with exchange metadata for routing information
  - [ ] 3.2 Implement `MultiExchangePath` extending SellSelection with exchange routing and profit tracking across exchanges
  - [ ] 3.3 Add `multiExchangeSelection` method to Calculator accepting NonEmptyList[Exchange[M]] for cross-exchange analysis
  - [ ] 3.4 Enhance SellSeed type to include exchange routing information for tree traversal
  - [ ] 3.5 Modify recursion scheme coalgebra to consider cross-exchange paths in tree generation
  - [ ] 3.6 Implement cross-exchange profit comparison logic with exchange-specific fee calculations
  - [ ] 3.7 Add exchange-aware tree node generation in SellTree for multi-exchange path optimization

- [ ] 4.0 Implement Multi-Exchange Configuration Management  
  - [ ] 4.1 Create `ExchangeConfig` case class with Optional[KrakenConfig], Optional[BinanceConfig], and ExchangePreferences
  - [ ] 4.2 Implement `ExchangePreferences` with timeout, maxDepthPerExchange, preferredOrder, minProfitThreshold configuration
  - [ ] 4.3 Add environment variable support for BINANCE_API_KEY, BINANCE_SECRET_KEY, BINANCE_TESTNET using Ciris
  - [ ] 4.4 Update Main.scala to load multi-exchange configuration with Secret types and proper redaction
  - [ ] 4.5 Implement exchange preference configuration: EXCHANGE_TIMEOUT_MS, MAX_DEPTH_PER_EXCHANGE, PREFERRED_EXCHANGES
  - [ ] 4.6 Add configurable exchange selection weights and minimum profit thresholds per exchange

- [ ] 5.0 Create Comprehensive Testing Suite and Documentation
  - [ ] 5.1 Create unit tests for Exchange abstraction layer with ScalaCheck properties for exchange selection logic
  - [ ] 5.2 Implement Binance integration tests with mocked API responses covering all order types and error scenarios  
  - [ ] 5.3 Add Calculator multi-exchange tests using property-based testing for recursion scheme correctness
  - [ ] 5.4 Create integration tests against Binance testnet API for end-to-end verification
  - [ ] 5.5 Implement security tests for credential handling, redaction, and HMAC signature validation
  - [ ] 5.6 Add performance tests ensuring <2x performance impact and <30% memory increase from baseline
  - [ ] 5.7 Create comprehensive documentation including API integration guides, configuration examples, and monitoring setup
