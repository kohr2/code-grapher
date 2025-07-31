"""
Database Configuration for Graph Operations
"""
import os
from dataclasses import dataclass
from typing import Optional


@dataclass
class DatabaseConfig:
    """Configuration for Neo4j database connection"""
    
    uri: str = "bolt://localhost:7687"
    username: str = "neo4j"
    password: str = "password"
    max_connection_lifetime: int = 3600  # 1 hour
    max_connection_pool_size: int = 50
    connection_acquisition_timeout: int = 60
    
    @classmethod
    def from_environment(cls) -> 'DatabaseConfig':
        """Create configuration from environment variables"""
        return cls(
            uri=os.getenv("NEO4J_URL", cls.uri),
            username=os.getenv("NEO4J_USERNAME", cls.username),
            password=os.getenv("NEO4J_PASSWORD", cls.password),
            max_connection_lifetime=int(os.getenv("NEO4J_MAX_CONNECTION_LIFETIME", cls.max_connection_lifetime)),
            max_connection_pool_size=int(os.getenv("NEO4J_MAX_POOL_SIZE", cls.max_connection_pool_size)),
            connection_acquisition_timeout=int(os.getenv("NEO4J_CONNECTION_TIMEOUT", cls.connection_acquisition_timeout))
        )
    
    def validate(self) -> bool:
        """Validate configuration parameters"""
        if not self.uri or not self.username or not self.password:
            return False
        if self.max_connection_pool_size <= 0:
            return False
        if self.connection_acquisition_timeout <= 0:
            return False
        return True