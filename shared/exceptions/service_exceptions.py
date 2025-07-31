"""
Service-layer exception classes
"""


class ServiceException(Exception):
    """Base exception for service-layer errors"""
    pass


class ServiceNotRegistered(ServiceException):
    """Raised when trying to access a service that hasn't been registered"""
    pass


class ServiceInitializationError(ServiceException):
    """Raised when service initialization fails"""
    pass


class ServiceConfigurationError(ServiceException):
    """Raised when service configuration is invalid"""
    pass


class ServiceDependencyError(ServiceException):
    """Raised when service dependencies are not met"""
    pass


class CircularDependencyError(ServiceException):
    """Raised when circular dependencies are detected"""
    pass