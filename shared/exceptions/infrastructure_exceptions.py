"""
Infrastructure-layer exception classes
"""


class InfrastructureException(Exception):
    """Base exception for infrastructure-layer errors"""
    pass


class DatabaseConnectionError(InfrastructureException):
    """Raised when database connection fails"""
    pass


class ConfigurationError(InfrastructureException):
    """Raised when configuration is invalid or missing"""
    pass


class NetworkError(InfrastructureException):
    """Raised when network operations fail"""
    pass


class FileSystemError(InfrastructureException):
    """Raised when file system operations fail"""
    pass