"""
Data models for evaluation tracking
"""
from dataclasses import dataclass
from typing import Any, Dict, Optional
from datetime import datetime
from enum import Enum


class EvaluationCategory(Enum):
    """Categories for AI evaluation tracking"""
    RELATIONSHIP_EXTRACTION = "relationship_extraction"
    DESCRIPTION_GENERATION = "description_generation"
    SEMANTIC_SEARCH = "semantic_search"
    PERFORMANCE = "performance"
    ERROR_HANDLING = "error_handling"


class Sentiment(Enum):
    """Sentiment values for evaluation"""
    POSITIVE = "positive"
    NEUTRAL = "neutral"
    NEGATIVE = "negative"


@dataclass
class EvaluationResult:
    """Single evaluation result"""
    category: EvaluationCategory
    result: Any
    timestamp: datetime
    metadata: Optional[Dict[str, Any]] = None
    sentiment: Optional[Sentiment] = None
    confidence: Optional[float] = None
    
    def __post_init__(self):
        """Set timestamp if not provided"""
        if not hasattr(self, 'timestamp') or self.timestamp is None:
            self.timestamp = datetime.now()


@dataclass
class EvaluationSummary:
    """Summary of evaluations for a category"""
    category: EvaluationCategory
    total_evaluations: int
    success_rate: float
    average_confidence: Optional[float]
    sentiment_breakdown: Dict[str, int]
    latest_evaluation: Optional[datetime]
    performance_metrics: Optional[Dict[str, float]]