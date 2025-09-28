#!/usr/bin/env python3
"""
COBOL Support CLI

Command-line interface for COBOL analysis and processing.
"""

import argparse
import asyncio
import json
import logging
import sys
from pathlib import Path

from .cobol_integration import COBOLIntegration, analyze_cobol_file, start_cobol_background_service

def setup_logging(verbose: bool = False):
    """Setup logging configuration."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )

def print_results(results: dict, output_format: str = 'text'):
    """Print analysis results in the specified format."""
    if output_format == 'json':
        print(json.dumps(results, indent=2))
    else:
        # Text format
        summary = results.get('summary', {})
        print(f"\n=== COBOL Analysis Results ===")
        print(f"Overall Accuracy: {summary.get('overall_accuracy', 0):.1f}%")
        print(f"Expected Entities: {summary.get('total_expected_entities', 0)}")
        print(f"Actual Entities: {summary.get('total_actual_entities', 0)}")
        
        if summary.get('critical_issues'):
            print(f"\nCritical Issues:")
            for issue in summary['critical_issues']:
                print(f"  - {issue}")
        
        entity_breakdown = summary.get('entity_breakdown', {})
        if entity_breakdown:
            print(f"\nEntity Breakdown:")
            for entity_type, info in entity_breakdown.items():
                print(f"  {entity_type}: {info['actual']}/{info['expected']} ({info['accuracy']:.1f}%)")
        
        recommendations = summary.get('recommendations', [])
        if recommendations:
            print(f"\nRecommendations:")
            for i, rec in enumerate(recommendations, 1):
                print(f"  {i}. {rec}")

async def cmd_analyze(args):
    """Analyze a single COBOL file."""
    if not Path(args.file).exists():
        print(f"Error: File {args.file} does not exist")
        sys.exit(1)
    
    try:
        results = await analyze_cobol_file(args.file)
        print_results(results, args.format)
        
        if args.output:
            with open(args.output, 'w') as f:
                json.dump(results, f, indent=2)
            print(f"\nResults saved to {args.output}")
            
    except Exception as e:
        print(f"Error analyzing file: {e}")
        sys.exit(1)

async def cmd_background(args):
    """Start background COBOL processing service."""
    integration = await start_cobol_background_service(args.workers)
    
    print(f"COBOL background service started with {args.workers} workers")
    print("Press Ctrl+C to stop...")
    
    try:
        while True:
            stats = integration.get_processing_stats()
            queue_status = integration.get_queue_status()
            
            print(f"\rProcessed: {stats.get('tasks_processed', 0)}, "
                  f"Success: {stats.get('successful_tasks', 0)}, "
                  f"Failed: {stats.get('failed_tasks', 0)}, "
                  f"Pending: {queue_status.get('pending_tasks', 0)}", end='')
            
            await asyncio.sleep(5)
            
    except KeyboardInterrupt:
        print("\nStopping background service...")
        await integration.stop_background_processing()
        print("Background service stopped")

async def cmd_submit(args):
    """Submit COBOL files for background processing."""
    integration = await start_cobol_background_service(args.workers)
    
    files_submitted = 0
    for file_path in args.files:
        if Path(file_path).exists():
            integration.submit_cobol_task(file_path, args.priority)
            files_submitted += 1
            print(f"Submitted: {file_path}")
        else:
            print(f"Warning: File {file_path} does not exist")
    
    print(f"\nSubmitted {files_submitted} files for processing")
    
    # Wait for processing
    if args.wait:
        print("Waiting for processing to complete...")
        await asyncio.sleep(args.wait)
        
        stats = integration.get_processing_stats()
        print(f"Processing complete: {stats}")
    
    await integration.stop_background_processing()

def cmd_stats(args):
    """Show entity counts for COBOL files."""
    from .services import EnhancedCOBOLParser
    
    parser = EnhancedCOBOLParser()
    
    for file_path in args.files:
        if Path(file_path).exists():
            results = parser.parse_file(file_path)
            counts = results.get('counts', {})
            
            print(f"\n{file_path}:")
            for entity_type, count in counts.items():
                print(f"  {entity_type}: {count}")
        else:
            print(f"Warning: File {file_path} does not exist")

def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description='COBOL Analysis and Processing CLI',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Analyze a single COBOL file
  python -m cobol-support.cli analyze program.cbl

  # Analyze with JSON output
  python -m cobol-support.cli analyze program.cbl --format json --output results.json

  # Start background processing service
  python -m cobol-support.cli background --workers 4

  # Submit files for background processing
  python -m cobol-support.cli submit *.cbl --priority 2 --wait 30

  # Show entity counts
  python -m cobol-support.cli stats *.cbl
        """
    )
    
    parser.add_argument('-v', '--verbose', action='store_true',
                       help='Enable verbose logging')
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # Analyze command
    analyze_parser = subparsers.add_parser('analyze', help='Analyze a COBOL file')
    analyze_parser.add_argument('file', help='COBOL file to analyze')
    analyze_parser.add_argument('--format', choices=['text', 'json'], default='text',
                               help='Output format')
    analyze_parser.add_argument('--output', help='Output file for results')
    
    # Background command
    background_parser = subparsers.add_parser('background', help='Start background service')
    background_parser.add_argument('--workers', type=int, default=2,
                                  help='Number of worker threads')
    
    # Submit command
    submit_parser = subparsers.add_parser('submit', help='Submit files for processing')
    submit_parser.add_argument('files', nargs='+', help='COBOL files to process')
    submit_parser.add_argument('--workers', type=int, default=2,
                              help='Number of worker threads')
    submit_parser.add_argument('--priority', type=int, default=1,
                              help='Task priority')
    submit_parser.add_argument('--wait', type=int, help='Wait time in seconds')
    
    # Stats command
    stats_parser = subparsers.add_parser('stats', help='Show entity counts')
    stats_parser.add_argument('files', nargs='+', help='COBOL files to analyze')
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        sys.exit(1)
    
    setup_logging(args.verbose)
    
    # Run the appropriate command
    if args.command == 'analyze':
        asyncio.run(cmd_analyze(args))
    elif args.command == 'background':
        asyncio.run(cmd_background(args))
    elif args.command == 'submit':
        asyncio.run(cmd_submit(args))
    elif args.command == 'stats':
        cmd_stats(args)

if __name__ == "__main__":
    main()