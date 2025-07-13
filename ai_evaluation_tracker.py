import json
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any
from enum import Enum
from logger import logger, log_info, log_debug

class EvaluationCategory(Enum):
    """Categories for AI Lego Bricks evaluation"""
    PERFORMANCE = "performance"
    ACCURACY = "accuracy"
    USABILITY = "usability"
    RELIABILITY = "reliability"
    FEATURE_GAP = "feature_gap"
    BUG = "bug"
    SUGGESTION = "suggestion"

class Sentiment(Enum):
    """Sentiment of the evaluation"""
    POSITIVE = "positive"
    NEGATIVE = "negative"
    NEUTRAL = "neutral"

class AIEvaluationTracker:
    """Track and analyze AI Lego Bricks usage and effectiveness"""
    
    def __init__(self, storage_path: str = "ai_evaluations"):
        self.storage_path = Path(storage_path)
        self.storage_path.mkdir(exist_ok=True)
        self.evaluation_file = self.storage_path / "evaluations.json"
        self.summary_file = self.storage_path / "evaluation_summary.json"
        self.evaluations = self._load_evaluations()
        
        log_info(f"AI Evaluation Tracker initialized with {len(self.evaluations)} existing evaluations")
    
    def _load_evaluations(self) -> List[Dict[str, Any]]:
        """Load existing evaluations from file"""
        if self.evaluation_file.exists():
            with open(self.evaluation_file, 'r') as f:
                return json.load(f)
        return []
    
    def _save_evaluations(self):
        """Save evaluations to file"""
        with open(self.evaluation_file, 'w') as f:
            json.dump(self.evaluations, f, indent=2, default=str)
        log_debug(f"Saved {len(self.evaluations)} evaluations to {self.evaluation_file}")
    
    def record_evaluation(self, 
                         component: str,
                         description: str,
                         category: EvaluationCategory,
                         sentiment: Sentiment,
                         impact_score: int = 5,  # 1-10 scale
                         details: Optional[Dict[str, Any]] = None):
        """Record a new evaluation of AI Lego Bricks"""
        
        evaluation = {
            "timestamp": datetime.now().isoformat(),
            "component": component,
            "description": description,
            "category": category.value,
            "sentiment": sentiment.value,
            "impact_score": impact_score,
            "details": details or {}
        }
        
        self.evaluations.append(evaluation)
        self._save_evaluations()
        
        # Log the evaluation
        logger.log_ai_evaluation(
            component=component,
            evaluation=f"{sentiment.value.upper()}: {description} (Impact: {impact_score}/10)",
            category=category.value
        )
        
        logger.log_info(f"Recorded evaluation #{len(self.evaluations)}: {component} - {category.value}")
    
    def record_success(self, component: str, description: str, 
                      time_saved: Optional[float] = None,
                      accuracy: Optional[float] = None):
        """Record a successful use of AI Lego Bricks"""
        details = {}
        if time_saved:
            details["time_saved_seconds"] = time_saved
        if accuracy:
            details["accuracy_percentage"] = accuracy
            
        self.record_evaluation(
            component=component,
            description=f"SUCCESS: {description}",
            category=EvaluationCategory.PERFORMANCE,
            sentiment=Sentiment.POSITIVE,
            impact_score=8,
            details=details
        )
    
    def record_failure(self, component: str, description: str,
                      error_type: Optional[str] = None,
                      workaround: Optional[str] = None):
        """Record a failure or limitation of AI Lego Bricks"""
        details = {}
        if error_type:
            details["error_type"] = error_type
        if workaround:
            details["workaround"] = workaround
            
        self.record_evaluation(
            component=component,
            description=f"FAILURE: {description}",
            category=EvaluationCategory.BUG,
            sentiment=Sentiment.NEGATIVE,
            impact_score=7,
            details=details
        )
    
    def record_observation(self, component: str, observation: str,
                          category: EvaluationCategory = EvaluationCategory.USABILITY):
        """Record a general observation about AI Lego Bricks"""
        self.record_evaluation(
            component=component,
            description=observation,
            category=category,
            sentiment=Sentiment.NEUTRAL,
            impact_score=5
        )
    
    def generate_summary(self) -> Dict[str, Any]:
        """Generate a summary of all evaluations"""
        if not self.evaluations:
            return {"message": "No evaluations recorded yet"}
        
        summary = {
            "total_evaluations": len(self.evaluations),
            "evaluation_period": {
                "start": min(e["timestamp"] for e in self.evaluations),
                "end": max(e["timestamp"] for e in self.evaluations)
            },
            "by_category": {},
            "by_sentiment": {},
            "by_component": {},
            "average_impact_score": 0,
            "high_impact_issues": [],
            "top_successes": [],
            "critical_failures": []
        }
        
        # Categorize evaluations
        for eval in self.evaluations:
            # By category
            cat = eval["category"]
            if cat not in summary["by_category"]:
                summary["by_category"][cat] = 0
            summary["by_category"][cat] += 1
            
            # By sentiment
            sent = eval["sentiment"]
            if sent not in summary["by_sentiment"]:
                summary["by_sentiment"][sent] = 0
            summary["by_sentiment"][sent] += 1
            
            # By component
            comp = eval["component"]
            if comp not in summary["by_component"]:
                summary["by_component"][comp] = {"count": 0, "avg_impact": 0}
            summary["by_component"][comp]["count"] += 1
            
            # Track high impact items
            if eval["impact_score"] >= 8:
                if eval["sentiment"] == "positive":
                    summary["top_successes"].append({
                        "component": eval["component"],
                        "description": eval["description"],
                        "impact": eval["impact_score"]
                    })
                elif eval["sentiment"] == "negative":
                    summary["critical_failures"].append({
                        "component": eval["component"],
                        "description": eval["description"],
                        "impact": eval["impact_score"]
                    })
        
        # Calculate averages
        total_impact = sum(e["impact_score"] for e in self.evaluations)
        summary["average_impact_score"] = total_impact / len(self.evaluations)
        
        # Component impact averages
        for comp, data in summary["by_component"].items():
            comp_evals = [e for e in self.evaluations if e["component"] == comp]
            avg_impact = sum(e["impact_score"] for e in comp_evals) / len(comp_evals)
            data["avg_impact"] = round(avg_impact, 2)
        
        # Sort lists by impact
        summary["top_successes"].sort(key=lambda x: x["impact"], reverse=True)
        summary["critical_failures"].sort(key=lambda x: x["impact"], reverse=True)
        
        # Limit to top 5
        summary["top_successes"] = summary["top_successes"][:5]
        summary["critical_failures"] = summary["critical_failures"][:5]
        
        # Save summary
        with open(self.summary_file, 'w') as f:
            json.dump(summary, f, indent=2)
        
        logger.log_info(f"Generated evaluation summary with {summary['total_evaluations']} evaluations")
        
        return summary
    
    def get_recommendations(self) -> List[str]:
        """Generate recommendations based on evaluations"""
        recommendations = []
        
        summary = self.generate_summary()
        
        # Check sentiment balance
        sentiments = summary["by_sentiment"]
        if sentiments.get("negative", 0) > sentiments.get("positive", 0):
            recommendations.append("Consider alternative tools - negative experiences outweigh positive")
        
        # Check for repeated failures
        failure_components = {}
        for eval in self.evaluations:
            if eval["sentiment"] == "negative":
                comp = eval["component"]
                if comp not in failure_components:
                    failure_components[comp] = 0
                failure_components[comp] += 1
        
        for comp, count in failure_components.items():
            if count >= 3:
                recommendations.append(f"Component '{comp}' has {count} failures - needs attention or replacement")
        
        # Check categories
        categories = summary["by_category"]
        if categories.get("bug", 0) > 5:
            recommendations.append("High bug count - consider reporting issues to maintainers")
        
        if categories.get("feature_gap", 0) > 3:
            recommendations.append("Multiple feature gaps identified - may need custom implementations")
        
        logger.log_info(f"Generated {len(recommendations)} recommendations")
        
        return recommendations

# Global tracker instance
ai_tracker = AIEvaluationTracker()

# Convenience functions
def track_success(component: str, description: str, **kwargs):
    ai_tracker.record_success(component, description, **kwargs)

def track_failure(component: str, description: str, **kwargs):
    ai_tracker.record_failure(component, description, **kwargs)

def track_observation(component: str, observation: str, **kwargs):
    ai_tracker.record_observation(component, observation, **kwargs)