"""
Data models for relationship extraction
"""

from dataclasses import dataclass
from typing import List, Optional, Dict, Any
from enum import Enum


class RelationshipType(Enum):
    """Types of relationships between code entities"""

    CALLS = "CALLS"
    INHERITS = "INHERITS"
    USES = "USES"
    IMPLEMENTS = "IMPLEMENTS"
    DEPENDS_ON = "DEPENDS_ON"
    OVERRIDES = "OVERRIDES"
    DECORATES = "DECORATES"
    INSTANTIATES = "INSTANTIATES"
    DEFINES = "DEFINES"
    CONFIGURES = "CONFIGURES"
    TRANSFORMS = "TRANSFORMS"
    VALIDATES = "VALIDATES"
    DATA_FLOW = "DATA_FLOW"
    STATE_MUTATION = "STATE_MUTATION"
    IMPORTS = "IMPORTS"
    EXPORTS = "EXPORTS"
    EVENT_HANDLING = "EVENT_HANDLING"
    
    # COBOL-specific relationships
    INCLUDES = "INCLUDES"  # COPY statement relationships
    PASSES_DATA = "PASSES_DATA"  # Parameter passing (BY REFERENCE/VALUE)
    HANDLES_ERRORS = "HANDLES_ERRORS"  # USE statement connections
    USES_QUEUE = "USES_QUEUE"  # Communication relationships
    BINDS_SCREEN = "BINDS_SCREEN"  # Screen data connections
    PERFORMS = "PERFORMS"  # PERFORM statement relationships
    REPLACES = "REPLACES"  # REPLACING phrase relationships


class ConfidenceLevel(Enum):
    """Confidence levels for extracted relationships"""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"


@dataclass
class Relationship:
    """Represents a relationship between code entities"""

    source_entity: str
    target_entity: str
    relationship_type: RelationshipType
    confidence: float  # 0.0 to 1.0
    confidence_level: ConfidenceLevel
    source_file: str
    target_file: str
    line_number: Optional[int] = None
    context: Optional[str] = None
    properties: Optional[Dict[str, Any]] = None

    def __post_init__(self):
        """Set confidence level based on confidence score"""
        if self.confidence >= 0.8:
            self.confidence_level = ConfidenceLevel.HIGH
        elif self.confidence >= 0.5:
            self.confidence_level = ConfidenceLevel.MEDIUM
        else:
            self.confidence_level = ConfidenceLevel.LOW

    def is_valid(self) -> bool:
        """Check if relationship is valid"""
        return (
            self.source_entity
            and self.target_entity
            and self.source_entity != self.target_entity
            and 0.0 <= self.confidence <= 1.0
        )


@dataclass
class RelationshipExtractionResult:
    """Result of relationship extraction process"""

    relationships: List[Relationship]
    source_file: str
    target_file: str
    extraction_time: float
    total_found: int
    total_valid: int
    validation_errors: List[str]
    success: bool
    error: Optional[str] = None

    def get_valid_relationships(self) -> List[Relationship]:
        """Get only valid relationships"""
        return [r for r in self.relationships if r.is_valid()]

    def get_relationships_by_type(self, relationship_type: RelationshipType) -> List[Relationship]:
        """Get relationships of specific type"""
        return [r for r in self.relationships if r.relationship_type == relationship_type]

    def get_high_confidence_relationships(self) -> List[Relationship]:
        """Get only high confidence relationships"""
        return [r for r in self.relationships if r.confidence_level == ConfidenceLevel.HIGH]
