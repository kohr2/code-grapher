"""
MLflow Dashboard Setup and Visualization Configuration for Code Grapher

[2025-07-12 10:45:30] [MLFLOW_DASHBOARD] [INFO] Creating MLflow dashboard configuration
and visualization setup for Code Grapher agent pipeline monitoring.

This module provides:
1. Dashboard startup and configuration utilities
2. Custom visualization templates for agent pipeline flows
3. Real-time monitoring dashboard creation
4. Performance trends and comparison views
5. Error tracking and debugging interfaces
"""

import os
import sys
import json
import time
import subprocess
import webbrowser
from typing import Dict, Any, Optional, List
from pathlib import Path
from datetime import datetime, timedelta

import mlflow
from mlflow.tracking import MlflowClient
import pandas as pd

from logger import logger, CodeGrapherLogger
from ai_evaluation_tracker import ai_tracker
from mlflow_config import mlflow_config


class MLflowDashboardSetup:
    """
    MLflow dashboard setup and configuration manager for Code Grapher.
    
    Provides tools for:
    - Dashboard server management
    - Custom visualization creation
    - Real-time monitoring setup
    - Performance analysis views
    """
    
    def __init__(self):
        self.session_logger = logger.create_session_logger("MLflowDashboard")
        self.client = MlflowClient()
        
        # Dashboard configuration
        self.dashboard_config = {
            "experiment_name": "CodeGrapher-Agent-Pipeline",
            "refresh_interval_seconds": 30,
            "auto_open_browser": True,
            "custom_views": {
                "pipeline_overview": True,
                "agent_performance": True,
                "error_tracking": True,
                "resource_usage": True
            }
        }
        
        self.session_logger.log_info("MLflow Dashboard Setup initialized")
    
    def create_dashboard_url(self, base_url: Optional[str] = None) -> str:
        """
        Create a customized MLflow dashboard URL with filters and views.
        
        Args:
            base_url: Base MLflow server URL (defaults to current tracking URI)
            
        Returns:
            Customized dashboard URL
        """
        if not base_url:
            tracking_uri = mlflow.get_tracking_uri()
            if tracking_uri.startswith("file://"):
                # File-based tracking, need to check if server is running
                base_url = mlflow_config.get_dashboard_url()
                if not base_url:
                    raise ValueError("MLflow server not running. Start server first.")
            else:
                base_url = tracking_uri
        
        try:
            # Get experiment ID
            experiment = self.client.get_experiment_by_name(self.dashboard_config["experiment_name"])
            if not experiment:
                raise ValueError(f"Experiment '{self.dashboard_config['experiment_name']}' not found")
            
            experiment_id = experiment.experiment_id
            
            # Build dashboard URL with filters
            dashboard_url = f"{base_url}/#/experiments/{experiment_id}"
            
            # Add query parameters for enhanced view
            params = {
                "searchFilter": "",
                "orderByKey": "start_time",
                "orderByAsc": "false",
                "startTime": "ALL",
                "lifecycleFilter": "Active"
            }
            
            # Add parameters to URL
            param_string = "&".join([f"{k}={v}" for k, v in params.items()])
            dashboard_url += f"?{param_string}"
            
            self.session_logger.log_decision(
                decision=f"Created dashboard URL: {dashboard_url}",
                reasoning="Built URL with experiment ID and useful default filters",
                alternatives=["Use basic MLflow URL", "Create multiple specialized URLs"]
            )
            
            return dashboard_url
            
        except Exception as e:
            self.session_logger.log_error(e, {"base_url": base_url})
            
            ai_tracker.record_failure(
                component="mlflow-dashboard-url",
                description=f"Failed to create dashboard URL: {str(e)}",
                error_type=type(e).__name__
            )
            
            # Fallback to basic URL
            return base_url
    
    def start_dashboard_server_with_config(self) -> Dict[str, Any]:
        """
        Start MLflow server with dashboard-optimized configuration.
        
        Returns:
            Server startup status and configuration
        """
        self.session_logger.log_operation_start("MLflowDashboard.start_server")
        
        server_info = {
            "started": False,
            "url": None,
            "process_id": None,
            "config_applied": False
        }
        
        try:
            # Use mlflow_config to start server
            from mlflow_config import mlflow_config
            setup_result = mlflow_config.start_mlflow_server()
            
            if setup_result.get("started", False):
                server_info.update(setup_result)
                server_info["config_applied"] = True
                
                self.session_logger.log_operation_end(
                    "MLflowDashboard.start_server",
                    duration=1.0,
                    success=True,
                    details=server_info
                )
                
                ai_tracker.record_success(
                    component="mlflow-dashboard-server",
                    description=f"Successfully started MLflow dashboard server at {server_info['dashboard_url']}",
                    time_saved=1.0
                )
            else:
                server_info["error"] = setup_result.get("error", "Unknown error")
                
                self.session_logger.log_operation_end(
                    "MLflowDashboard.start_server",
                    duration=1.0,
                    success=False,
                    details=server_info
                )
            
            return server_info
            
        except Exception as e:
            server_info["error"] = str(e)
            
            self.session_logger.log_error(e, {"context": "dashboard_server_start"})
            
            ai_tracker.record_failure(
                component="mlflow-dashboard-server",
                description=f"Failed to start dashboard server: {str(e)}",
                error_type=type(e).__name__
            )
            
            return server_info
    
    def open_dashboard_in_browser(self, url: Optional[str] = None) -> bool:
        """
        Open MLflow dashboard in web browser.
        
        Args:
            url: Dashboard URL (if not provided, will be generated)
            
        Returns:
            True if successfully opened
        """
        try:
            if not url:
                url = self.create_dashboard_url()
            
            # Wait a moment for server to be ready
            time.sleep(2)
            
            # Open in browser
            webbrowser.open(url)
            
            self.session_logger.log_info(f"Opened MLflow dashboard in browser: {url}")
            
            ai_tracker.record_success(
                component="mlflow-dashboard-browser",
                description=f"Successfully opened dashboard in browser",
                time_saved=0.5
            )
            
            return True
            
        except Exception as e:
            self.session_logger.log_error(e, {"url": url})
            
            ai_tracker.record_failure(
                component="mlflow-dashboard-browser",
                description=f"Failed to open dashboard in browser: {str(e)}",
                error_type=type(e).__name__
            )
            
            return False
    
    def create_pipeline_overview_report(self) -> Dict[str, Any]:
        """
        Create a comprehensive pipeline overview report from MLflow data.
        
        Returns:
            Pipeline overview report with statistics and visualizations
        """
        self.session_logger.log_operation_start("MLflowDashboard.create_pipeline_overview")
        
        report = {
            "generated_at": datetime.now().isoformat(),
            "experiment_name": self.dashboard_config["experiment_name"],
            "pipeline_statistics": {},
            "agent_performance": {},
            "recent_executions": [],
            "error_analysis": {},
            "recommendations": []
        }
        
        try:
            # Get experiment
            experiment = self.client.get_experiment_by_name(self.dashboard_config["experiment_name"])
            if not experiment:
                report["error"] = f"Experiment '{self.dashboard_config['experiment_name']}' not found"
                return report
            
            # Get recent runs (last 50)
            runs = self.client.search_runs(
                experiment_ids=[experiment.experiment_id],
                max_results=50,
                order_by=["start_time DESC"]
            )
            
            if not runs:
                report["pipeline_statistics"]["total_runs"] = 0
                report["recommendations"].append("No pipeline runs found. Execute a workflow to see dashboard data.")
                return report
            
            # Analyze pipeline statistics
            report["pipeline_statistics"] = self._analyze_pipeline_statistics(runs)
            
            # Analyze agent performance
            report["agent_performance"] = self._analyze_agent_performance(runs)
            
            # Get recent executions
            report["recent_executions"] = self._get_recent_executions(runs[:10])
            
            # Analyze errors
            report["error_analysis"] = self._analyze_errors(runs)
            
            # Generate recommendations
            report["recommendations"] = self._generate_recommendations(report)
            
            self.session_logger.log_operation_end(
                "MLflowDashboard.create_pipeline_overview",
                duration=1.0,
                success=True,
                details={
                    "total_runs": len(runs),
                    "successful_runs": report["pipeline_statistics"].get("successful_runs", 0)
                }
            )
            
            return report
            
        except Exception as e:
            report["error"] = str(e)
            
            self.session_logger.log_error(e, {"context": "pipeline_overview"})
            
            ai_tracker.record_failure(
                component="mlflow-dashboard-overview",
                description=f"Failed to create pipeline overview: {str(e)}",
                error_type=type(e).__name__
            )
            
            return report
    
    def _analyze_pipeline_statistics(self, runs: List[Any]) -> Dict[str, Any]:
        """Analyze overall pipeline statistics from runs"""
        stats = {
            "total_runs": len(runs),
            "successful_runs": 0,
            "failed_runs": 0,
            "avg_duration_seconds": 0,
            "total_execution_time": 0,
            "success_rate_percent": 0,
            "runs_last_24h": 0,
            "runs_last_7d": 0
        }
        
        if not runs:
            return stats
        
        now = datetime.now()
        total_duration = 0
        duration_count = 0
        
        for run in runs:
            # Count success/failure
            if run.info.status == "FINISHED":
                stats["successful_runs"] += 1
            else:
                stats["failed_runs"] += 1
            
            # Calculate duration if available
            if run.info.end_time and run.info.start_time:
                duration = (run.info.end_time - run.info.start_time) / 1000  # Convert to seconds
                total_duration += duration
                duration_count += 1
            
            # Count recent runs
            if run.info.start_time:
                run_time = datetime.fromtimestamp(run.info.start_time / 1000)
                if now - run_time < timedelta(hours=24):
                    stats["runs_last_24h"] += 1
                if now - run_time < timedelta(days=7):
                    stats["runs_last_7d"] += 1
        
        # Calculate averages
        if duration_count > 0:
            stats["avg_duration_seconds"] = total_duration / duration_count
            stats["total_execution_time"] = total_duration
        
        if stats["total_runs"] > 0:
            stats["success_rate_percent"] = (stats["successful_runs"] / stats["total_runs"]) * 100
        
        return stats
    
    def _analyze_agent_performance(self, runs: List[Any]) -> Dict[str, Any]:
        """Analyze individual agent performance from nested runs"""
        agent_stats = {}
        
        for run in runs:
            # Check if this is a pipeline run (has nested runs)
            if run.info.run_id:
                try:
                    # Get nested runs (agent runs)
                    nested_runs = self.client.search_runs(
                        experiment_ids=[run.info.experiment_id],
                        filter_string=f"tags.parent_run = '{run.info.run_id}'"
                    )
                    
                    for nested_run in nested_runs:
                        agent_id = nested_run.data.tags.get("agent_id")
                        agent_type = nested_run.data.tags.get("agent_type")
                        
                        if agent_id:
                            if agent_id not in agent_stats:
                                agent_stats[agent_id] = {
                                    "agent_type": agent_type,
                                    "total_executions": 0,
                                    "successful_executions": 0,
                                    "failed_executions": 0,
                                    "avg_duration_seconds": 0,
                                    "total_duration": 0,
                                    "success_rate_percent": 0
                                }
                            
                            agent_stats[agent_id]["total_executions"] += 1
                            
                            if nested_run.info.status == "FINISHED":
                                agent_stats[agent_id]["successful_executions"] += 1
                            else:
                                agent_stats[agent_id]["failed_executions"] += 1
                            
                            # Calculate duration
                            if nested_run.info.end_time and nested_run.info.start_time:
                                duration = (nested_run.info.end_time - nested_run.info.start_time) / 1000
                                agent_stats[agent_id]["total_duration"] += duration
                
                except Exception as e:
                    self.session_logger.log_error(e, {"run_id": run.info.run_id})
        
        # Calculate averages
        for agent_id, stats in agent_stats.items():
            if stats["total_executions"] > 0:
                stats["avg_duration_seconds"] = stats["total_duration"] / stats["total_executions"]
                stats["success_rate_percent"] = (stats["successful_executions"] / stats["total_executions"]) * 100
        
        return agent_stats
    
    def _get_recent_executions(self, runs: List[Any]) -> List[Dict[str, Any]]:
        """Get summary of recent pipeline executions"""
        executions = []
        
        for run in runs:
            execution = {
                "run_id": run.info.run_id,
                "workflow_name": run.data.tags.get("workflow_name", "unknown"),
                "status": run.info.status,
                "start_time": datetime.fromtimestamp(run.info.start_time / 1000).isoformat() if run.info.start_time else None,
                "end_time": datetime.fromtimestamp(run.info.end_time / 1000).isoformat() if run.info.end_time else None,
                "duration_seconds": None,
                "agents_executed": run.data.metrics.get("agents_executed", 0),
                "errors_count": run.data.metrics.get("errors_count", 0)
            }
            
            if run.info.end_time and run.info.start_time:
                execution["duration_seconds"] = (run.info.end_time - run.info.start_time) / 1000
            
            executions.append(execution)
        
        return executions
    
    def _analyze_errors(self, runs: List[Any]) -> Dict[str, Any]:
        """Analyze error patterns from runs"""
        error_analysis = {
            "total_errors": 0,
            "error_types": {},
            "error_trends": {},
            "most_common_errors": []
        }
        
        for run in runs:
            if run.info.status != "FINISHED":
                error_analysis["total_errors"] += 1
                
                # Get error information from parameters
                error_type = run.data.params.get("pipeline_error_type", "unknown")
                error_message = run.data.params.get("pipeline_error_message", "")
                
                if error_type not in error_analysis["error_types"]:
                    error_analysis["error_types"][error_type] = 0
                error_analysis["error_types"][error_type] += 1
        
        # Sort errors by frequency
        error_analysis["most_common_errors"] = sorted(
            error_analysis["error_types"].items(),
            key=lambda x: x[1],
            reverse=True
        )[:5]
        
        return error_analysis
    
    def _generate_recommendations(self, report: Dict[str, Any]) -> List[str]:
        """Generate recommendations based on pipeline analysis"""
        recommendations = []
        
        stats = report.get("pipeline_statistics", {})
        agent_perf = report.get("agent_performance", {})
        errors = report.get("error_analysis", {})
        
        # Success rate recommendations
        success_rate = stats.get("success_rate_percent", 0)
        if success_rate < 80:
            recommendations.append(f"Pipeline success rate is {success_rate:.1f}%. Consider reviewing error patterns and improving error handling.")
        elif success_rate > 95:
            recommendations.append("Excellent pipeline reliability! Consider documenting best practices.")
        
        # Performance recommendations
        avg_duration = stats.get("avg_duration_seconds", 0)
        if avg_duration > 300:  # 5 minutes
            recommendations.append(f"Average pipeline duration is {avg_duration:.1f}s. Consider optimizing slow agents or adding parallelization.")
        
        # Agent-specific recommendations
        for agent_id, agent_stats in agent_perf.items():
            if agent_stats.get("success_rate_percent", 0) < 70:
                recommendations.append(f"Agent '{agent_id}' has low success rate ({agent_stats['success_rate_percent']:.1f}%). Review its implementation.")
            
            if agent_stats.get("avg_duration_seconds", 0) > 60:
                recommendations.append(f"Agent '{agent_id}' is slow (avg {agent_stats['avg_duration_seconds']:.1f}s). Consider optimization.")
        
        # Error pattern recommendations
        if errors.get("total_errors", 0) > stats.get("total_runs", 0) * 0.2:
            recommendations.append("High error rate detected. Review error analysis and implement better error handling.")
        
        most_common_error = errors.get("most_common_errors", [])
        if most_common_error:
            error_type, count = most_common_error[0]
            recommendations.append(f"Most common error is '{error_type}' ({count} occurrences). Focus on resolving this issue.")
        
        return recommendations
    
    def save_dashboard_config(self, config_file: str) -> None:
        """Save dashboard configuration to file"""
        try:
            config_data = {
                "mlflow_dashboard": self.dashboard_config,
                "generated_at": datetime.now().isoformat()
            }
            
            with open(config_file, 'w') as f:
                json.dump(config_data, f, indent=2)
            
            self.session_logger.log_info(f"Saved dashboard configuration to {config_file}")
            
        except Exception as e:
            self.session_logger.log_error(e, {"config_file": config_file})
    
    def export_pipeline_report(self, output_file: str, format: str = "json") -> bool:
        """
        Export pipeline overview report to file.
        
        Args:
            output_file: Output file path
            format: Export format ("json", "html", "csv")
            
        Returns:
            True if export successful
        """
        try:
            report = self.create_pipeline_overview_report()
            
            if format.lower() == "json":
                with open(output_file, 'w') as f:
                    json.dump(report, f, indent=2, default=str)
            
            elif format.lower() == "html":
                html_content = self._generate_html_report(report)
                with open(output_file, 'w') as f:
                    f.write(html_content)
            
            elif format.lower() == "csv":
                # Export key statistics as CSV
                df = pd.DataFrame([report["pipeline_statistics"]])
                df.to_csv(output_file, index=False)
            
            else:
                raise ValueError(f"Unsupported export format: {format}")
            
            self.session_logger.log_info(f"Exported pipeline report to {output_file} ({format})")
            
            ai_tracker.record_success(
                component="mlflow-dashboard-export",
                description=f"Successfully exported pipeline report to {format}",
                time_saved=1.0
            )
            
            return True
            
        except Exception as e:
            self.session_logger.log_error(e, {
                "output_file": output_file,
                "format": format
            })
            
            ai_tracker.record_failure(
                component="mlflow-dashboard-export",
                description=f"Failed to export pipeline report: {str(e)}",
                error_type=type(e).__name__
            )
            
            return False
    
    def _generate_html_report(self, report: Dict[str, Any]) -> str:
        """Generate HTML report from pipeline data"""
        html_template = f"""
<!DOCTYPE html>
<html>
<head>
    <title>Code Grapher Pipeline Report</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; }}
        .header {{ background-color: #f4f4f4; padding: 20px; border-radius: 5px; }}
        .section {{ margin: 20px 0; padding: 15px; border: 1px solid #ddd; border-radius: 5px; }}
        .metric {{ display: inline-block; margin: 10px; padding: 10px; background-color: #e8f4fd; border-radius: 3px; }}
        .error {{ color: #d32f2f; }}
        .success {{ color: #388e3c; }}
        table {{ width: 100%; border-collapse: collapse; }}
        th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
        th {{ background-color: #f2f2f2; }}
    </style>
</head>
<body>
    <div class="header">
        <h1>Code Grapher Pipeline Report</h1>
        <p>Generated: {report.get('generated_at', 'Unknown')}</p>
        <p>Experiment: {report.get('experiment_name', 'Unknown')}</p>
    </div>
    
    <div class="section">
        <h2>Pipeline Statistics</h2>
        {self._format_statistics_html(report.get('pipeline_statistics', {}))}
    </div>
    
    <div class="section">
        <h2>Agent Performance</h2>
        {self._format_agent_performance_html(report.get('agent_performance', {}))}
    </div>
    
    <div class="section">
        <h2>Recent Executions</h2>
        {self._format_recent_executions_html(report.get('recent_executions', []))}
    </div>
    
    <div class="section">
        <h2>Recommendations</h2>
        <ul>
        {''.join([f'<li>{rec}</li>' for rec in report.get('recommendations', [])])}
        </ul>
    </div>
</body>
</html>
        """
        
        return html_template
    
    def _format_statistics_html(self, stats: Dict[str, Any]) -> str:
        """Format pipeline statistics as HTML"""
        if not stats:
            return "<p>No statistics available</p>"
        
        metrics_html = ""
        for key, value in stats.items():
            if isinstance(value, float):
                formatted_value = f"{value:.2f}"
            else:
                formatted_value = str(value)
            
            metrics_html += f'<div class="metric"><strong>{key.replace("_", " ").title()}:</strong> {formatted_value}</div>'
        
        return metrics_html
    
    def _format_agent_performance_html(self, agent_perf: Dict[str, Any]) -> str:
        """Format agent performance as HTML table"""
        if not agent_perf:
            return "<p>No agent performance data available</p>"
        
        html = "<table><tr><th>Agent ID</th><th>Type</th><th>Executions</th><th>Success Rate</th><th>Avg Duration</th></tr>"
        
        for agent_id, stats in agent_perf.items():
            html += f"""
            <tr>
                <td>{agent_id}</td>
                <td>{stats.get('agent_type', 'Unknown')}</td>
                <td>{stats.get('total_executions', 0)}</td>
                <td>{stats.get('success_rate_percent', 0):.1f}%</td>
                <td>{stats.get('avg_duration_seconds', 0):.2f}s</td>
            </tr>
            """
        
        html += "</table>"
        return html
    
    def _format_recent_executions_html(self, executions: List[Dict[str, Any]]) -> str:
        """Format recent executions as HTML table"""
        if not executions:
            return "<p>No recent executions found</p>"
        
        html = "<table><tr><th>Workflow</th><th>Status</th><th>Start Time</th><th>Duration</th><th>Agents</th><th>Errors</th></tr>"
        
        for execution in executions[:10]:  # Show only first 10
            status_class = "success" if execution.get("status") == "FINISHED" else "error"
            duration = execution.get("duration_seconds", 0)
            duration_str = f"{duration:.1f}s" if duration else "N/A"
            
            html += f"""
            <tr>
                <td>{execution.get('workflow_name', 'Unknown')}</td>
                <td class="{status_class}">{execution.get('status', 'Unknown')}</td>
                <td>{execution.get('start_time', 'N/A')}</td>
                <td>{duration_str}</td>
                <td>{execution.get('agents_executed', 0)}</td>
                <td>{execution.get('errors_count', 0)}</td>
            </tr>
            """
        
        html += "</table>"
        return html


# Global dashboard setup instance
mlflow_dashboard = MLflowDashboardSetup()

# Convenience functions
def start_dashboard_server() -> Dict[str, Any]:
    """Start MLflow dashboard server"""
    return mlflow_dashboard.start_dashboard_server_with_config()

def open_dashboard() -> bool:
    """Open MLflow dashboard in browser"""
    return mlflow_dashboard.open_dashboard_in_browser()

def create_pipeline_report() -> Dict[str, Any]:
    """Create pipeline overview report"""
    return mlflow_dashboard.create_pipeline_overview_report()

def export_pipeline_report(output_file: str, format: str = "json") -> bool:
    """Export pipeline report to file"""
    return mlflow_dashboard.export_pipeline_report(output_file, format)