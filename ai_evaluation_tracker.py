"""
AI Evaluation Tracker for monitoring AI operations and performance.
"""
import time
from typing import Dict, Any, List, Optional
from dataclasses import dataclass, field
from enum import Enum
from logger import logger


class EvaluationCategory(Enum):
    """Categories for AI evaluation"""
    ACCURACY = "accuracy"
    PERFORMANCE = "performance"
    QUALITY = "quality"
    RELEVANCE = "relevance"
    COMPLETENESS = "completeness"


class Sentiment(Enum):
    """Sentiment classifications"""
    POSITIVE = "positive"
    NEGATIVE = "negative"
    NEUTRAL = "neutral"


@dataclass
class EvaluationMetric:
    """Single evaluation metric"""
    name: str
    value: Any
    timestamp: float = field(default_factory=time.time)
    metadata: Dict[str, Any] = field(default_factory=dict)


class AIEvaluationTracker:
    """Tracks AI operations and performance metrics"""
    
    def __init__(self):
        self.metrics: List[EvaluationMetric] = []
        self.session_id = str(int(time.time()))
        logger.log_info(f"AI Evaluation Tracker initialized for session: {self.session_id}")
    
    def track_operation(self, operation: str, result: Any, duration: float = None, metadata: Dict[str, Any] = None):
        """Track an AI operation"""
        metric_data = {
            "operation": operation,
            "result": result,
            "duration": duration,
            "session_id": self.session_id
        }
        
        if metadata:
            metric_data.update(metadata)
        
        metric = EvaluationMetric(
            name=f"ai_operation_{operation}",
            value=result,
            metadata=metric_data
        )
        
        self.metrics.append(metric)
        logger.log_debug(f"Tracked AI operation: {operation} (duration: {duration}s)")
    
    def track_accuracy(self, operation: str, expected: Any, actual: Any, accuracy_score: float = None):
        """Track accuracy metrics"""
        if accuracy_score is None and expected is not None and actual is not None:
            # Simple accuracy calculation
            accuracy_score = 1.0 if expected == actual else 0.0
        
        metric = EvaluationMetric(
            name=f"ai_accuracy_{operation}",
            value=accuracy_score,
            metadata={
                "expected": expected,
                "actual": actual,
                "operation": operation,
                "session_id": self.session_id
            }
        )
        
        self.metrics.append(metric)
        logger.log_debug(f"Tracked accuracy for {operation}: {accuracy_score}")
    
    def track_performance(self, operation: str, duration: float, tokens_used: int = None, cost: float = None):
        """Track performance metrics"""
        metric_data = {
            "operation": operation,
            "duration": duration,
            "session_id": self.session_id
        }
        
        if tokens_used is not None:
            metric_data["tokens_used"] = tokens_used
        
        if cost is not None:
            metric_data["cost"] = cost
        
        metric = EvaluationMetric(
            name=f"ai_performance_{operation}",
            value=duration,
            metadata=metric_data
        )
        
        self.metrics.append(metric)
        logger.log_debug(f"Tracked performance for {operation}: {duration}s")
    
    def get_summary(self) -> Dict[str, Any]:
        """Get summary of tracked metrics"""
        if not self.metrics:
            return {"total_operations": 0, "session_id": self.session_id}
        
        operations = {}
        total_duration = 0
        accuracy_scores = []
        
        for metric in self.metrics:
            op_name = metric.metadata.get("operation", "unknown")
            
            if op_name not in operations:
                operations[op_name] = {"count": 0, "total_duration": 0}
            
            operations[op_name]["count"] += 1
            
            if "duration" in metric.metadata and metric.metadata["duration"]:
                duration = metric.metadata["duration"]
                operations[op_name]["total_duration"] += duration
                total_duration += duration
            
            if metric.name.startswith("ai_accuracy_") and metric.value is not None:
                accuracy_scores.append(metric.value)
        
        return {
            "session_id": self.session_id,
            "total_operations": len(self.metrics),
            "total_duration": total_duration,
            "average_accuracy": sum(accuracy_scores) / len(accuracy_scores) if accuracy_scores else None,
            "operations": operations
        }
    
    def log_summary(self):
        """Log a summary of tracked metrics"""
        summary = self.get_summary()
        logger.log_info(f"AI Evaluation Summary: {summary}")


# Global instance
ai_tracker = AIEvaluationTracker()