"""
Connection Manager - Enhanced connection pooling for Neo4j
"""
import os
from typing import Optional
from contextlib import contextmanager
from neo4j import GraphDatabase, Driver
from shared.services.service_locator import ServiceLocator


class ConnectionManager:
    """Manages Neo4j connections with pooling"""
    
    def __init__(self, uri: Optional[str] = None, 
                 username: Optional[str] = None, 
                 password: Optional[str] = None):
        self.uri = uri or os.getenv("NEO4J_URL", "bolt://localhost:7687")
        self.username = username or os.getenv("NEO4J_USERNAME", "neo4j")
        self.password = password or os.getenv("NEO4J_PASSWORD", "password")
        self.logger = ServiceLocator.get_logger("connection_manager")
        
        # Connection pool configuration
        self._driver: Optional[Driver] = None
        self._max_connection_lifetime = 3600  # 1 hour
        self._max_connection_pool_size = 50
        self._connection_acquisition_timeout = 60
        
    def initialize(self) -> None:
        """Initialize the connection pool"""
        if self._driver is None:
            try:
                self._driver = GraphDatabase.driver(
                    self.uri,
                    auth=(self.username, self.password),
                    max_connection_lifetime=self._max_connection_lifetime,
                    max_connection_pool_size=self._max_connection_pool_size,
                    connection_acquisition_timeout=self._connection_acquisition_timeout
                )
                
                # Verify connectivity
                self._driver.verify_connectivity()
                self.logger.log_info("Connection pool initialized successfully")
                
            except Exception as e:
                self.logger.log_error(f"Failed to initialize connection pool: {e}")
                raise
    
    @contextmanager
    def get_session(self):
        """Get a session from the connection pool"""
        if self._driver is None:
            self.initialize()
            
        session = None
        try:
            session = self._driver.session()
            yield session
        except Exception as e:
            self.logger.log_error(f"Session error: {e}")
            raise
        finally:
            if session:
                session.close()
    
    def close(self) -> None:
        """Close the connection pool"""
        if self._driver:
            self._driver.close()
            self._driver = None
            self.logger.log_info("Connection pool closed")
    
    def __del__(self):
        """Cleanup on destruction"""
        self.close()