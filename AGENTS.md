# AGENTS.md

## Project Overview

**Arbo** is a sophisticated cryptocurrency trading bot implemented in Scala using functional programming principles and advanced recursion schemes. The bot is specifically designed for the Kraken cryptocurrency exchange and uses complex algorithms to find optimal sell paths through currency pairs to maximize profit.

## Architecture

### Core Technologies
- **Scala 2.13.1** - Primary programming language
- **Cats Effect 2.1.2** - Effect system for managing side effects
- **Droste 0.8.0** - Recursion schemes library for advanced tree traversal algorithms
- **Http4s 0.21.2** - HTTP client/server library for API interactions
- **Circe 0.13.0** - JSON parsing and encoding
- **Ciris 1.0.4** - Configuration management
- **Monocle 2.0.4** - Optics library for functional data manipulation

### Project Structure
```
src/main/scala/arbo/
├── Main.scala              # Application entry point and configuration
├── Calculator.scala        # Core trading algorithm using recursion schemes
├── Security.scala          # Cryptographic utilities
├── data/                   # Domain models and type definitions
│   ├── package.scala       # Type aliases and domain types
│   ├── SellOrder.scala     # Trading order model
│   ├── SellSeed.scala      # Algorithm seed state
│   ├── SellSelection.scala # Selection result types
│   └── SellTree.scala      # Tree structure for algorithm
├── kraken/                 # Kraken exchange API integration
│   ├── Config.scala        # API configuration
│   ├── RestClient.scala    # HTTP client for Kraken API
│   ├── Order.scala         # Kraken order types
│   ├── Assets.scala        # Asset definitions
│   └── SalesResponse.scala # API response models
├── server/                 # HTTP server for bot control
│   ├── ArboServer.scala    # Server setup and configuration
│   └── ArboRoutes.scala    # HTTP routes
├── elgot/                  # Custom recursion scheme implementations
│   └── package.scala       # Elgot morphism definitions
└── cache/                  # Caching functionality
    └── Keep.scala          # Cache implementation
```

### Key Architectural Patterns

1. **Functional Programming**: Pure functions, immutable data structures, and side-effect isolation
2. **Recursion Schemes**: Uses Droste for complex tree traversal and optimization algorithms
3. **Elgot Morphisms**: Custom recursion scheme allowing early termination in recursive computations
4. **Effect System**: All side effects managed through Cats Effect IO monad
5. **Type Safety**: Extensive use of ADTs and type-level programming
6. **Configuration as Code**: Environment-based configuration with compile-time safety

### Algorithm Architecture

The core trading algorithm uses **Elgot morphisms** (a form of recursion scheme) to traverse a tree of possible trading paths and find the optimal sequence of trades:

- **SellTree**: Represents the tree structure of possible trading paths
- **SellSeed**: State carried through recursive computation
- **ElgotCoalgebraM**: Builds the tree structure with early termination
- **Algebra**: Processes tree nodes to find optimal paths

## Setup Commands

### Prerequisites
- **JDK 8 or higher**
- **SBT 1.3.9** (specified in project/build.properties)
- **Environment variables**: `API_KEY` and `PRIVATE_KEY` for Kraken API access

### Development Setup
```bash
# Install dependencies
sbt compile

# Run the application
sbt run

# Run tests
sbt test

# Run with auto-reload during development
sbt ~reRun

# Generate coverage report
sbt clean coverage test coverageReport

# Format code
sbt scalafmt

# Package for deployment
sbt universal:packageBin
```

## Environment Configuration

The application requires these environment variables:

```bash
export API_KEY="your_kraken_api_key"
export PRIVATE_KEY="base64_encoded_private_key"
```

**Security Note**: Private keys are handled as `Secret` types and automatically redacted in logs.

## Code Style and Conventions

### Functional Programming Guidelines
- **Pure functions**: Functions should be referentially transparent when possible
- **Immutable data**: Use immutable data structures; avoid var declarations
- **Effect isolation**: Wrap side effects in IO or other effect types
- **Type safety**: Leverage Scala's type system extensively

### Naming Conventions
- **Domain types**: Clear, descriptive names (e.g., `SellOrder`, `Holding`)
- **Effect functions**: Use F[_] or M[_] for generic effect types
- **Recursion schemes**: Follow Droste conventions (Algebra, Coalgebra, etc.)

### Code Organization
- **Package objects**: Use for type aliases and common imports
- **ADTs**: Model domain concepts as algebraic data types
- **Optics**: Use Monocle lenses for data access and modification
- **Type classes**: Implement instances for common type classes (Eq, Show, etc.)

### Comments and Documentation
- Document complex recursion scheme usage
- Explain business logic in trading algorithms
- Use scaladoc for public APIs
- Inline comments for non-obvious functional patterns

## Testing Strategy

### Test Structure
```
src/test/scala/arbo/
├── CalculatorSpec.scala     # Core algorithm tests
├── SecuritySpec.scala       # Cryptographic function tests
├── data/                    # Domain model tests
└── kraken/                  # API client tests
```

### Testing Guidelines
- **Property-based testing**: Use ScalaCheck for algorithm verification
- **Effect testing**: Test IO computations properly
- **Mock external APIs**: Don't hit real Kraken API in tests
- **Test type class laws**: Verify mathematical properties

### Running Tests
```bash
# Run all tests
sbt test

# Run specific test file
sbt "testOnly arbo.CalculatorSpec"

# Run with coverage
sbt clean coverage test coverageReport

# Continuous testing
sbt ~test
```

## API Integration

### Kraken API
- **Base URL**: Uses Kraken REST API v0
- **Authentication**: HMAC-SHA512 signature with API key/secret
- **Rate limiting**: Be mindful of Kraken's rate limits
- **Error handling**: Robust error handling for network failures

### Security Considerations
- **API keys**: Never commit API keys to version control
- **Private keys**: Stored as ByteVector and handled securely
- **Logging**: Sensitive data automatically redacted in logs
- **Configuration**: Use Ciris Secret types for sensitive config

## Development Workflow

### Local Development
1. Set up environment variables
2. Start SBT in continuous compilation mode: `sbt ~compile`
3. Use `sbt ~reRun` for auto-restart on changes
4. Run tests frequently: `sbt ~test`

### Code Quality
- **Scalafmt**: Automatic code formatting configured
- **Scoverage**: Code coverage reporting
- **Tpolecat**: Comprehensive compiler flags for safety
- **Wartremover**: Additional compile-time checks (if configured)

### Debugging
- **Logging**: Uses Logback with structured logging
- **Effect debugging**: Leverage Cats Effect debugging tools
- **REPL**: Use `sbt console` for interactive development

## Deployment

### Production Considerations
- **Resource management**: Proper cleanup of ExecutionContexts and HTTP clients
- **Monitoring**: Add metrics for trading performance and API calls
- **Error recovery**: Implement circuit breakers for external API calls
- **Graceful shutdown**: Ensure proper cleanup on application termination

### Performance
- **Memory usage**: Monitor for memory leaks in recursive computations
- **CPU usage**: Recursion schemes can be computationally intensive
- **API rate limits**: Implement appropriate backoff strategies
- **Caching**: Use provided cache infrastructure for API responses

## Troubleshooting

### Common Issues
- **OutOfMemoryError**: Large trading trees can consume significant memory
- **API authentication failures**: Check API key format and permissions
- **Stack overflow**: Deep recursion in tree traversal (adjust JVM stack size)
- **Configuration errors**: Verify environment variables are set correctly

### Debugging Tools
- **JVM profiling**: Use tools like JProfiler for performance analysis
- **Cats Effect debugging**: Enable fiber dumps and effect tracing
- **HTTP debugging**: Log HTTP requests/responses for API troubleshooting

## Security Notes

- **Never log API keys or private keys**
- **Use Ciris Secret types** for all sensitive configuration
- **Validate all external inputs** from Kraken API
- **Implement proper error handling** to avoid information leakage
- **Regular security audits** of dependencies

## Dependencies and Versions

All dependency versions are pinned in `build.sbt`. Key libraries:
- Cats Effect for effect management
- Droste for recursion schemes
- Http4s for HTTP client/server
- Circe for JSON handling
- Ciris for configuration
- Monocle for functional data access

## Performance Considerations

- **Tree depth limits**: Algorithm has configurable maximum search depth
- **Memoization**: Consider caching for repeated calculations
- **Parallel processing**: Leverage Cats Effect for concurrent API calls
- **Memory management**: Be aware of large data structures in recursion

---

*This AGENTS.md follows the standard from [agents.md](https://agents.md/) to provide comprehensive guidance for AI coding agents working on the Arbo cryptocurrency trading bot.*
