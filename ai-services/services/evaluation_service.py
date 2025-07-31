"""
Evaluation service that wraps the existing ai_evaluation_tracker.py
"""
from typing import Any, Dict, List, Optional
from datetime import datetime
from evaluation_tracker_interface import EvaluationTrackerInterface
from evaluation_models import EvaluationResult, EvaluationCategory, Sentiment, EvaluationSummary
from shared.interfaces.logger_interface import LoggerInterface


class EvaluationService(EvaluationTrackerInterface):
    """Service for tracking AI evaluation metrics that wraps existing ai_evaluation_tracker"""
    
    def __init__(self, logger: Optional[LoggerInterface] = None):
        """Initialize evaluation service"""
        self.logger = logger
        self._legacy_tracker = None
        self._evaluation_history: List[EvaluationResult] = []
        self._initialize_legacy_tracker()
    
    def _initialize_legacy_tracker(self) -> None:
        """Initialize the legacy AI evaluation tracker"""
        try:
            from ai_evaluation_tracker import ai_tracker
            self._legacy_tracker = ai_tracker
            
            if self.logger:
                self.logger.log_info("Initialized legacy AI evaluation tracker")
        except Exception as e:
            if self.logger:
                self.logger.log_warning(f"Failed to initialize legacy tracker: {e}")
    
    def track_evaluation(self, category: EvaluationCategory, result: Any,
                        metadata: Optional[Dict[str, Any]] = None) -> None:
        """Track an AI evaluation result"""
        try:
            # Create evaluation result
            evaluation = EvaluationResult(
                category=category,
                result=result,
                timestamp=datetime.now(),
                metadata=metadata or {},
                sentiment=self._determine_sentiment(result),
                confidence=self._extract_confidence(result, metadata)
            )
            
            # Add to history
            self._evaluation_history.append(evaluation)
            
            # Delegate to legacy tracker if available
            if self._legacy_tracker:
                try:
                    # Map our categories to legacy tracker categories
                    legacy_category = self._map_to_legacy_category(category)
                    self._legacy_tracker.track_evaluation(legacy_category, result)
                except Exception as e:
                    if self.logger:
                        self.logger.log_warning(f"Failed to track in legacy system: {e}")
            
            if self.logger:
                self.logger.log_debug(f"Tracked evaluation: {category.value} - {evaluation.sentiment}")
                
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to track evaluation: {e}")
    
    def get_evaluation_summary(self, category: Optional[EvaluationCategory] = None) -> Dict[str, Any]:
        """Get summary of evaluations"""
        try:
            # Filter by category if specified
            evaluations = self._evaluation_history
            if category:
                evaluations = [e for e in evaluations if e.category == category]
            
            if not evaluations:
                return {
                    "total_evaluations": 0,
                    "success_rate": 0.0,
                    "average_confidence": None,
                    "sentiment_breakdown": {},
                    "latest_evaluation": None
                }
            
            # Calculate metrics
            total_evaluations = len(evaluations)
            successful_evaluations = len([e for e in evaluations if self._is_successful(e)])
            success_rate = successful_evaluations / total_evaluations if total_evaluations > 0 else 0.0
            
            # Calculate average confidence
            confidences = [e.confidence for e in evaluations if e.confidence is not None]
            average_confidence = sum(confidences) / len(confidences) if confidences else None
            
            # Sentiment breakdown
            sentiment_breakdown = {}
            for evaluation in evaluations:
                if evaluation.sentiment:
                    sentiment_key = evaluation.sentiment.value
                    sentiment_breakdown[sentiment_key] = sentiment_breakdown.get(sentiment_key, 0) + 1
            
            # Latest evaluation
            latest_evaluation = max(evaluations, key=lambda e: e.timestamp).timestamp if evaluations else None
            
            summary = {
                "total_evaluations": total_evaluations,
                "success_rate": success_rate,
                "average_confidence": average_confidence,
                "sentiment_breakdown": sentiment_breakdown,
                "latest_evaluation": latest_evaluation,
                "category": category.value if category else "all"
            }
            
            # Add legacy tracker summary if available
            if self._legacy_tracker and hasattr(self._legacy_tracker, 'get_summary'):
                try:
                    legacy_summary = self._legacy_tracker.get_summary()
                    summary["legacy_tracker"] = legacy_summary
                except Exception as e:
                    if self.logger:
                        self.logger.log_warning(f"Failed to get legacy summary: {e}")
            
            return summary
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to get evaluation summary: {e}")
            return {"error": str(e)}
    
    def get_evaluation_history(self, limit: int = 100) -> List[EvaluationResult]:
        """Get evaluation history"""
        try:
            # Sort by timestamp (most recent first) and limit
            sorted_history = sorted(self._evaluation_history, key=lambda e: e.timestamp, reverse=True)
            return sorted_history[:limit]
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to get evaluation history: {e}")
            return []
    
    def clear_evaluations(self) -> None:
        """Clear all evaluation data"""
        try:
            self._evaluation_history.clear()
            
            # Clear legacy tracker if possible
            if self._legacy_tracker and hasattr(self._legacy_tracker, 'clear'):
                try:
                    self._legacy_tracker.clear()
                except Exception as e:
                    if self.logger:
                        self.logger.log_warning(f"Failed to clear legacy tracker: {e}")
            
            if self.logger:
                self.logger.log_info("Cleared all evaluation data")
                
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to clear evaluations: {e}")
    
    def _determine_sentiment(self, result: Any) -> Optional[Sentiment]:
        """Determine sentiment from evaluation result"""
        try:
            if isinstance(result, dict):
                # Check for explicit sentiment
                if 'sentiment' in result:
                    sentiment_str = str(result['sentiment']).lower()
                    if sentiment_str in ['positive', 'good', 'success']:
                        return Sentiment.POSITIVE
                    elif sentiment_str in ['negative', 'bad', 'error', 'failed']:
                        return Sentiment.NEGATIVE
                    else:
                        return Sentiment.NEUTRAL
                
                # Check for success indicators
                if 'success' in result:
                    return Sentiment.POSITIVE if result['success'] else Sentiment.NEGATIVE
                
                # Check for error indicators
                if 'error' in result or 'exception' in result:
                    return Sentiment.NEGATIVE
            
            elif isinstance(result, bool):
                return Sentiment.POSITIVE if result else Sentiment.NEGATIVE
            
            elif isinstance(result, (int, float)):
                if result > 0.7:
                    return Sentiment.POSITIVE
                elif result < 0.3:
                    return Sentiment.NEGATIVE
                else:
                    return Sentiment.NEUTRAL
            
            return Sentiment.NEUTRAL
            
        except Exception:
            return Sentiment.NEUTRAL
    
    def _extract_confidence(self, result: Any, metadata: Optional[Dict[str, Any]]) -> Optional[float]:
        """Extract confidence score from result or metadata"""
        try:
            # Check metadata first
            if metadata and 'confidence' in metadata:
                conf = metadata['confidence']
                if isinstance(conf, (int, float)) and 0 <= conf <= 1:
                    return float(conf)
            
            # Check result
            if isinstance(result, dict) and 'confidence' in result:
                conf = result['confidence']
                if isinstance(conf, (int, float)) and 0 <= conf <= 1:
                    return float(conf)
            
            # If result is a number between 0 and 1, treat as confidence
            if isinstance(result, (int, float)) and 0 <= result <= 1:
                return float(result)
            
            return None
            
        except Exception:
            return None
    
    def _is_successful(self, evaluation: EvaluationResult) -> bool:
        """Determine if an evaluation represents success"""
        # Check sentiment
        if evaluation.sentiment == Sentiment.POSITIVE:
            return True
        elif evaluation.sentiment == Sentiment.NEGATIVE:
            return False
        
        # Check confidence
        if evaluation.confidence is not None:
            return evaluation.confidence > 0.5
        
        # Check result
        if isinstance(evaluation.result, bool):
            return evaluation.result
        
        if isinstance(evaluation.result, dict):
            if 'success' in evaluation.result:
                return evaluation.result['success']
            if 'error' in evaluation.result:
                return False
        
        # Default to neutral/successful
        return True
    
    def _map_to_legacy_category(self, category: EvaluationCategory) -> str:
        """Map our category to legacy tracker category"""
        category_mapping = {
            EvaluationCategory.RELATIONSHIP_EXTRACTION: "relationship_extraction",
            EvaluationCategory.DESCRIPTION_GENERATION: "entity_description",
            EvaluationCategory.SEMANTIC_SEARCH: "semantic_search",
            EvaluationCategory.PERFORMANCE: "performance",
            EvaluationCategory.ERROR_HANDLING: "error_handling"
        }
        
        return category_mapping.get(category, category.value)
    
    def get_category_summary(self, category: EvaluationCategory) -> EvaluationSummary:
        """Get detailed summary for a specific category"""
        try:
            evaluations = [e for e in self._evaluation_history if e.category == category]
            
            if not evaluations:
                return EvaluationSummary(
                    category=category,
                    total_evaluations=0,
                    success_rate=0.0,
                    average_confidence=None,
                    sentiment_breakdown={},
                    latest_evaluation=None,
                    performance_metrics=None
                )
            
            # Calculate metrics
            total_evaluations = len(evaluations)
            successful = [e for e in evaluations if self._is_successful(e)]
            success_rate = len(successful) / total_evaluations
            
            # Average confidence
            confidences = [e.confidence for e in evaluations if e.confidence is not None]
            average_confidence = sum(confidences) / len(confidences) if confidences else None
            
            # Sentiment breakdown
            sentiment_breakdown = {}
            for evaluation in evaluations:
                if evaluation.sentiment:
                    key = evaluation.sentiment.value
                    sentiment_breakdown[key] = sentiment_breakdown.get(key, 0) + 1
            
            # Latest evaluation
            latest_evaluation = max(evaluations, key=lambda e: e.timestamp).timestamp
            
            return EvaluationSummary(
                category=category,
                total_evaluations=total_evaluations,
                success_rate=success_rate,
                average_confidence=average_confidence,
                sentiment_breakdown=sentiment_breakdown,
                latest_evaluation=latest_evaluation,
                performance_metrics=self._calculate_performance_metrics(evaluations)
            )
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to get category summary: {e}")
            
            return EvaluationSummary(
                category=category,
                total_evaluations=0,
                success_rate=0.0,
                average_confidence=None,
                sentiment_breakdown={},
                latest_evaluation=None,
                performance_metrics=None
            )
    
    def _calculate_performance_metrics(self, evaluations: List[EvaluationResult]) -> Optional[Dict[str, float]]:
        """Calculate performance metrics from evaluations"""
        try:
            if not evaluations:
                return None
            
            # Extract timing information if available
            durations = []
            for evaluation in evaluations:
                if evaluation.metadata and 'duration' in evaluation.metadata:
                    duration = evaluation.metadata['duration']
                    if isinstance(duration, (int, float)):
                        durations.append(float(duration))
            
            if not durations:
                return None
            
            return {
                "average_duration": sum(durations) / len(durations),
                "min_duration": min(durations),
                "max_duration": max(durations),
                "total_evaluations": len(evaluations)
            }
            
        except Exception:
            return None