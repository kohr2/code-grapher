"""
Logger facade implementation that wraps the existing logger module
"""
from typing import Any, Dict, Optional
from shared.interfaces.logger_interface import LoggerInterface

# Import the existing logger - this is the only place that directly imports it
from logger import logger as legacy_logger


class LoggerFacade(LoggerInterface):
    """Facade that wraps the existing logger implementation"""
    
    def __init__(self, name: str = "default"):
        self.name = name
        self._legacy_logger = legacy_logger
    
    def log_info(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log info level message"""
        self._legacy_logger.log_info(message)
        if extra:
            self._legacy_logger.log_info(f"Extra data: {extra}")
    
    def log_warning(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log warning level message"""
        self._legacy_logger.log_warning(message)
        if extra:
            self._legacy_logger.log_info(f"Extra data: {extra}")
    
    def log_error(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log error level message"""
        if isinstance(message, Exception):
            self._legacy_logger.log_error(message, extra or {})
        else:
            self._legacy_logger.log_error(message)
            if extra:
                self._legacy_logger.log_info(f"Extra data: {extra}")
    
    def log_debug(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log debug level message"""
        # Legacy logger doesn't have debug, use info
        self._legacy_logger.log_info(f"DEBUG: {message}")
        if extra:
            self._legacy_logger.log_info(f"Extra data: {extra}")
    
    def create_session_logger(self, session_name: str) -> 'LoggerInterface':
        """Create a session-specific logger"""
        # Delegate to legacy logger
        session_logger = self._legacy_logger.create_session_logger(session_name)
        return SessionLoggerFacade(session_logger, session_name)
    
    def log_operation_start(self, operation: str, context: Dict[str, Any]) -> None:
        """Log operation start"""
        self._legacy_logger.log_operation_start(operation, context)
    
    def log_operation_end(self, operation: str, context: Dict[str, Any] = None, **kwargs) -> None:
        """Log operation end"""
        # Handle different parameter styles for backward compatibility
        if context is None:
            context = {}
        
        # Legacy logger might expect different parameters
        try:
            self._legacy_logger.log_operation_end(operation, context)
        except TypeError:
            # Try with kwargs if direct call fails
            try:
                # Pass all kwargs to the legacy logger
                if hasattr(self._legacy_logger, 'log_operation_end'):
                    self._legacy_logger.log_operation_end(operation, **kwargs)
                else:
                    # Fallback to info logging
                    self._legacy_logger.log_info(f"OPERATION END: {operation} | Context: {context} | Extra: {kwargs}")
            except Exception:
                # Ultimate fallback
                self._legacy_logger.log_info(f"OPERATION END: {operation}")
    
    def log_performance(self, operation: str = None, duration: float = None, context: Optional[Dict[str, Any]] = None, 
                       metric: str = None, value: float = None, unit: str = None) -> None:
        """Log performance metrics - supports both old and new parameter styles"""
        try:
            # Handle new style parameters (metric, value, unit, context)
            if metric is not None and value is not None:
                # Convert to old style for compatibility
                operation = operation or metric
                duration = duration or (value / 1000.0 if unit == "ms" else value)
                
            # Try to call the legacy logger's log_performance method
            if hasattr(self._legacy_logger, 'log_performance'):
                self._legacy_logger.log_performance(operation, duration, context or {})
            else:
                # Fallback to regular info logging
                msg = f"PERFORMANCE: {operation} | Duration: {duration}"
                if unit:
                    msg += f" {unit}"
                if context:
                    msg += f" | Context: {context}"
                self._legacy_logger.log_info(msg)
        except Exception as e:
            # Ultimate fallback - just log the performance metric
            self._legacy_logger.log_info(f"PERFORMANCE: {operation or metric} - {duration or value}")


class SessionLoggerFacade(LoggerInterface):
    """Facade for session loggers"""
    
    def __init__(self, session_logger, session_name: str):
        self.session_logger = session_logger
        self.session_name = session_name
    
    def log_info(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log info level message"""
        self.session_logger.log_info(message)
        if extra:
            self.session_logger.log_info(f"Extra data: {extra}")
    
    def log_warning(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log warning level message"""
        self.session_logger.log_warning(message)
        if extra:
            self.session_logger.log_info(f"Extra data: {extra}")
    
    def log_error(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log error level message"""
        if isinstance(message, Exception):
            self.session_logger.log_error(message, extra or {})
        else:
            self.session_logger.log_error(message)
            if extra:
                self.session_logger.log_info(f"Extra data: {extra}")
    
    def log_debug(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log debug level message"""
        self.session_logger.log_info(f"DEBUG: {message}")
        if extra:
            self.session_logger.log_info(f"Extra data: {extra}")
    
    def create_session_logger(self, session_name: str) -> 'LoggerInterface':
        """Create a session-specific logger"""
        new_session_logger = self.session_logger.create_session_logger(session_name)
        return SessionLoggerFacade(new_session_logger, session_name)
    
    def log_operation_start(self, operation: str, context: Dict[str, Any]) -> None:
        """Log operation start"""
        self.session_logger.log_operation_start(operation, context)
    
    def log_operation_end(self, operation: str, context: Dict[str, Any] = None, **kwargs) -> None:
        """Log operation end"""
        # Handle different parameter styles for backward compatibility
        if context is None:
            context = {}
            
        # Legacy logger might expect different parameters
        try:
            self.session_logger.log_operation_end(operation, context)
        except TypeError:
            # Try with kwargs if direct call fails
            try:
                if hasattr(self.session_logger, 'log_operation_end'):
                    self.session_logger.log_operation_end(operation, **kwargs)
                else:
                    # Fallback to info logging
                    self.session_logger.log_info(f"OPERATION END: {operation} | Context: {context} | Extra: {kwargs}")
            except Exception:
                # Ultimate fallback
                self.session_logger.log_info(f"OPERATION END: {operation}")
    
    def log_performance(self, operation: str = None, duration: float = None, context: Optional[Dict[str, Any]] = None, 
                       metric: str = None, value: float = None, unit: str = None) -> None:
        """Log performance metrics - supports both old and new parameter styles"""
        try:
            # Handle new style parameters (metric, value, unit, context)
            if metric is not None and value is not None:
                # Convert to old style for compatibility
                operation = operation or metric
                duration = duration or (value / 1000.0 if unit == "ms" else value)
                
            # Try to call the legacy logger's log_performance method
            if hasattr(self.session_logger, 'log_performance'):
                self.session_logger.log_performance(operation, duration, context or {})
            else:
                # Fallback to regular info logging
                msg = f"PERFORMANCE: {operation} | Duration: {duration}"
                if unit:
                    msg += f" {unit}"
                if context:
                    msg += f" | Context: {context}"
                self.session_logger.log_info(msg)
        except Exception as e:
            # Ultimate fallback - just log the performance metric
            self.session_logger.log_info(f"PERFORMANCE: {operation or metric} - {duration or value}")
    
    def log_graph_action(self, action: str, entity_type: str, entity_name: str, relationships=None) -> None:
        """Log graph database actions"""
        try:
            # Try to call the legacy logger's log_graph_action method
            if hasattr(self.session_logger, 'log_graph_action'):
                self.session_logger.log_graph_action(action, entity_type, entity_name, relationships)
            else:
                # Fallback to regular info logging
                msg = f"GRAPH ACTION: {action} | Type: {entity_type} | Name: {entity_name}"
                if relationships:
                    msg += f" | Relationships: {relationships}"
                self.session_logger.log_info(msg)
        except Exception as e:
            # Ultimate fallback - just log the action
            self.session_logger.log_info(f"GRAPH ACTION: {action} for {entity_type}:{entity_name}")


class LoggerFactory:
    """Factory for creating logger instances"""
    
    _instance: Optional[LoggerInterface] = None
    
    @classmethod
    def get_logger(cls, name: str = "default") -> LoggerInterface:
        """Get a logger instance"""
        if cls._instance is None:
            cls._instance = LoggerFacade(name)
        return cls._instance
    
    @classmethod
    def set_logger(cls, logger: LoggerInterface) -> None:
        """Set a custom logger implementation"""
        cls._instance = logger