import ast
import os
import time
from typing import Dict, Any, List, Optional
from pathlib import Path
import json

from agents.base_agent import BaseAgent, AgentExecutionError, AgentConfigurationError


class CodeParserAgent(BaseAgent):
    """
    Agent for parsing source code files and extracting AST information
    Supports multiple programming languages with comprehensive metadata extraction
    """
    
    def __init__(self, agent_id: str, config: Dict[str, Any], shared_state: Optional[Dict[str, Any]] = None):
        super().__init__(agent_id, config, shared_state)
        
        # Agent-specific configuration
        self.supported_languages = config.get("config", {}).get("supportedLanguages", ["python"])
        self.parse_options = config.get("config", {}).get("parseOptions", {})
        self.max_file_size = config.get("config", {}).get("maxFileSize", 10485760)  # 10MB default
        self.encoding = config.get("config", {}).get("encoding", "utf-8")
        
        # Initialize language parsers
        self.parsers = self._initialize_parsers()
        
        self.session_logger.log_info(
            f"CodeParserAgent initialized with languages: {self.supported_languages}"
        )
    
    def _validate_configuration(self) -> None:
        """Validate agent-specific configuration"""
        config_section = self.config.get("config", {})
        
        if not config_section.get("supportedLanguages"):
            raise AgentConfigurationError("supportedLanguages is required")
        
        supported = config_section["supportedLanguages"]
        if not isinstance(supported, list) or not supported:
            raise AgentConfigurationError("supportedLanguages must be a non-empty list")
        
        # Validate that we can handle at least one language
        valid_languages = {"python", "javascript", "typescript", "java", "cpp", "c"}
        if not any(lang in valid_languages for lang in supported):
            raise AgentConfigurationError(f"No supported languages found. Valid: {valid_languages}")
    
    def get_capabilities(self) -> List[str]:
        """Return list of agent capabilities"""
        return ["file-reading", "ast-generation", "syntax-validation", "metadata-extraction"]
    
    def _initialize_parsers(self) -> Dict[str, Any]:
        """Initialize parsers for supported languages"""
        parsers = {}
        
        if "python" in self.supported_languages:
            parsers["python"] = self._create_python_parser()
        
        # For other languages, we would initialize appropriate parsers
        # For now, focusing on Python implementation
        
        return parsers
    
    def _create_python_parser(self) -> Dict[str, Any]:
        """Create Python-specific parser configuration"""
        return {
            "parser": ast,
            "extensions": [".py"],
            "parse_function": self._parse_python_content
        }
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute code parsing for provided files or directories"""
        
        self.session_logger.log_operation_start(
            "CodeParserAgent.execute",
            {
                "input_type": type(input_data).__name__,
                "input_keys": list(input_data.keys()) if input_data else []
            }
        )
        
        start_time = time.time()
        
        try:
            # Extract file paths from input
            files_to_parse = self._extract_file_paths(input_data)
            
            self.session_logger.log_decision(
                decision=f"Found {len(files_to_parse)} files to parse",
                reasoning="Extracted file paths from input data and filtered by supported extensions",
                alternatives=["Parse all files regardless of extension", "Use file content detection"]
            )
            
            # Parse each file
            parsed_results = []
            errors = []
            total_files = len(files_to_parse)
            
            for i, file_path in enumerate(files_to_parse):
                try:
                    self.session_logger.log_info(f"Parsing file {i+1}/{total_files}: {file_path}")
                    
                    parse_result = self._parse_single_file(file_path)
                    parsed_results.append(parse_result)
                    
                    # Update shared state with progress
                    self.update_shared_state(
                        "parsing_progress", 
                        {"completed": i + 1, "total": total_files, "current_file": file_path}
                    )
                    
                except Exception as e:
                    error_info = {
                        "file": file_path,
                        "error": str(e),
                        "error_type": type(e).__name__
                    }
                    errors.append(error_info)
                    
                    self.session_logger.log_error(e, {"file": file_path, "parsing_index": i})
            
            duration = time.time() - start_time
            
            # Compile results
            result = {
                "parsed_files": parsed_results,
                "total_files": total_files,
                "successful_parses": len(parsed_results),
                "failed_parses": len(errors),
                "errors": errors,
                "processing_time_seconds": duration,
                "languages_detected": self._get_detected_languages(parsed_results),
                "statistics": self._calculate_parsing_statistics(parsed_results)
            }
            
            # Log parsing results
            self.session_logger.log_code_analysis(
                "batch_parsing",
                {
                    "total_files": total_files,
                    "successful": len(parsed_results),
                    "failed": len(errors),
                    "duration": duration,
                    "languages": result["languages_detected"]
                }
            )
            
            # Log agent-specific metrics
            self.log_agent_specific_metrics({
                "files_parsed": len(parsed_results),
                "parse_errors": len(errors),
                "avg_file_size": sum(r.get("metadata", {}).get("fileSize", 0) for r in parsed_results) / max(len(parsed_results), 1),
                "total_lines": sum(r.get("metadata", {}).get("lineCount", 0) for r in parsed_results)
            })
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_error(e, {"input_data": input_data})
            raise AgentExecutionError(f"Code parsing failed: {str(e)}") from e
    
    def _extract_file_paths(self, input_data: Dict[str, Any]) -> List[str]:
        """Extract file paths from input data"""
        file_paths = []
        
        # Handle different input formats
        if "files" in input_data:
            # Direct file list
            files = input_data["files"]
            if isinstance(files, list):
                file_paths.extend(files)
            elif isinstance(files, str):
                file_paths.append(files)
        
        elif "directory" in input_data:
            # Directory scanning
            directory = input_data["directory"]
            recursive = input_data.get("recursive", True)
            file_paths.extend(self._scan_directory(directory, recursive))
        
        elif "codebase" in input_data and "files" in input_data["codebase"]:
            # Codebase structure
            file_paths.extend(input_data["codebase"]["files"])
        
        else:
            # Try to find any file-like inputs
            for key, value in input_data.items():
                if isinstance(value, str) and (value.endswith(('.py', '.js', '.ts', '.java', '.cpp', '.c'))):
                    file_paths.append(value)
                elif isinstance(value, list):
                    for item in value:
                        if isinstance(item, str) and os.path.isfile(item):
                            file_paths.append(item)
        
        # Filter by supported extensions and validate existence
        valid_files = []
        for file_path in file_paths:
            if self._is_supported_file(file_path) and os.path.isfile(file_path):
                valid_files.append(file_path)
        
        return valid_files
    
    def _scan_directory(self, directory: str, recursive: bool = True) -> List[str]:
        """Scan directory for source code files"""
        file_paths = []
        
        if not os.path.isdir(directory):
            self.session_logger.log_error(
                Exception(f"Directory not found: {directory}"),
                {"directory": directory, "recursive": recursive}
            )
            return file_paths
        
        try:
            if recursive:
                for root, dirs, files in os.walk(directory):
                    # Skip common non-source directories
                    dirs[:] = [d for d in dirs if not d.startswith('.') and d not in ['__pycache__', 'node_modules', 'build', 'dist']]
                    
                    for file in files:
                        file_path = os.path.join(root, file)
                        if self._is_supported_file(file_path):
                            file_paths.append(file_path)
            else:
                for item in os.listdir(directory):
                    item_path = os.path.join(directory, item)
                    if os.path.isfile(item_path) and self._is_supported_file(item_path):
                        file_paths.append(item_path)
        
        except Exception as e:
            self.session_logger.log_error(e, {"directory": directory, "recursive": recursive})
        
        return file_paths
    
    def _is_supported_file(self, file_path: str) -> bool:
        """Check if file is supported for parsing"""
        file_ext = Path(file_path).suffix.lower()
        
        # Map extensions to languages
        ext_to_lang = {
            ".py": "python",
            ".js": "javascript", 
            ".ts": "typescript",
            ".java": "java",
            ".cpp": "cpp",
            ".cc": "cpp",
            ".cxx": "cpp",
            ".c": "c",
            ".h": "c"
        }
        
        language = ext_to_lang.get(file_ext)
        return language is not None and language in self.supported_languages
    
    def _parse_single_file(self, file_path: str) -> Dict[str, Any]:
        """Parse a single source code file"""
        file_start_time = time.time()
        
        # Check file size
        file_size = os.path.getsize(file_path)
        if file_size > self.max_file_size:
            raise AgentExecutionError(f"File too large: {file_size} bytes (max: {self.max_file_size})")
        
        # Read file content
        try:
            with open(file_path, 'r', encoding=self.encoding) as f:
                content = f.read()
        except UnicodeDecodeError:
            # Try with different encoding
            with open(file_path, 'r', encoding='latin-1') as f:
                content = f.read()
            self.session_logger.log_decision(
                decision="Used latin-1 encoding as fallback",
                reasoning="UTF-8 decoding failed, attempting with latin-1",
                alternatives=["Skip file", "Try other encodings"]
            )
        
        # Detect language
        language = self._detect_language(file_path)
        
        # Parse based on language
        if language == "python":
            ast_data = self._parse_python_content(content, file_path)
        else:
            # For other languages, we would implement specific parsers
            # For now, provide basic metadata only
            ast_data = self._create_basic_ast(content, file_path, language)
        
        # Calculate metadata
        metadata = {
            "language": language,
            "fileSize": file_size,
            "lineCount": len(content.splitlines()),
            "encoding": self.encoding,
            "lastModified": os.path.getmtime(file_path),
            "parseTime": time.time() - file_start_time
        }
        
        return {
            "file_path": file_path,
            "ast": ast_data,
            "metadata": metadata,
            "content_preview": content[:500] if self.parse_options.get("includePreview", False) else None
        }
    
    def _detect_language(self, file_path: str) -> str:
        """Detect programming language from file extension"""
        ext_to_lang = {
            ".py": "python",
            ".js": "javascript",
            ".ts": "typescript", 
            ".java": "java",
            ".cpp": "cpp",
            ".cc": "cpp",
            ".cxx": "cpp",
            ".c": "c",
            ".h": "c"
        }
        
        file_ext = Path(file_path).suffix.lower()
        return ext_to_lang.get(file_ext, "unknown")
    
    def _parse_python_content(self, content: str, file_path: str) -> Dict[str, Any]:
        """Parse Python source code using AST"""
        try:
            tree = ast.parse(content, filename=file_path)
            
            # Extract AST information
            ast_info = {
                "type": "Module",
                "body": self._extract_ast_nodes(tree.body),
                "imports": self._extract_imports(tree),
                "classes": self._extract_classes(tree),
                "functions": self._extract_functions(tree),
                "variables": self._extract_variables(tree) if self.parse_options.get("extractVariables", False) else []
            }
            
            if self.parse_options.get("includePositions", True):
                ast_info["positions"] = self._extract_positions(tree)
            
            return ast_info
            
        except SyntaxError as e:
            # Handle syntax errors gracefully
            return {
                "type": "SyntaxError",
                "error": str(e),
                "line": e.lineno,
                "column": e.offset,
                "partial_parse": True
            }
    
    def _extract_ast_nodes(self, nodes: List[ast.AST]) -> List[Dict[str, Any]]:
        """Extract information from AST nodes"""
        extracted_nodes = []
        
        for node in nodes:
            node_info = {
                "type": type(node).__name__,
                "line": getattr(node, 'lineno', None),
                "column": getattr(node, 'col_offset', None)
            }
            
            # Add specific information based on node type
            if isinstance(node, ast.FunctionDef):
                node_info.update({
                    "name": node.name,
                    "args": [arg.arg for arg in node.args.args],
                    "decorators": [self._ast_to_string(dec) for dec in node.decorator_list],
                    "docstring": ast.get_docstring(node) if self.parse_options.get("extractDocstrings", True) else None
                })
            
            elif isinstance(node, ast.ClassDef):
                node_info.update({
                    "name": node.name,
                    "bases": [self._ast_to_string(base) for base in node.bases],
                    "decorators": [self._ast_to_string(dec) for dec in node.decorator_list],
                    "docstring": ast.get_docstring(node) if self.parse_options.get("extractDocstrings", True) else None,
                    "methods": [n.name for n in node.body if isinstance(n, ast.FunctionDef)]
                })
            
            elif isinstance(node, ast.Import):
                node_info.update({
                    "names": [alias.name for alias in node.names]
                })
            
            elif isinstance(node, ast.ImportFrom):
                node_info.update({
                    "module": node.module,
                    "names": [alias.name for alias in node.names],
                    "level": node.level
                })
            
            extracted_nodes.append(node_info)
        
        return extracted_nodes
    
    def _extract_imports(self, tree: ast.AST) -> List[Dict[str, Any]]:
        """Extract import statements"""
        imports = []
        
        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                for alias in node.names:
                    imports.append({
                        "type": "import",
                        "name": alias.name,
                        "alias": alias.asname,
                        "line": node.lineno
                    })
            
            elif isinstance(node, ast.ImportFrom):
                for alias in node.names:
                    imports.append({
                        "type": "from_import",
                        "module": node.module,
                        "name": alias.name,
                        "alias": alias.asname,
                        "level": node.level,
                        "line": node.lineno
                    })
        
        return imports
    
    def _extract_classes(self, tree: ast.AST) -> List[Dict[str, Any]]:
        """Extract class definitions"""
        classes = []
        
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                class_info = {
                    "name": node.name,
                    "line": node.lineno,
                    "column": node.col_offset,
                    "bases": [self._ast_to_string(base) for base in node.bases],
                    "decorators": [self._ast_to_string(dec) for dec in node.decorator_list],
                    "methods": [],
                    "docstring": ast.get_docstring(node) if self.parse_options.get("extractDocstrings", True) else None
                }
                
                # Extract methods
                for item in node.body:
                    if isinstance(item, ast.FunctionDef):
                        method_info = {
                            "name": item.name,
                            "line": item.lineno,
                            "args": [arg.arg for arg in item.args.args],
                            "decorators": [self._ast_to_string(dec) for dec in item.decorator_list],
                            "docstring": ast.get_docstring(item) if self.parse_options.get("extractDocstrings", True) else None
                        }
                        class_info["methods"].append(method_info)
                
                classes.append(class_info)
        
        return classes
    
    def _extract_functions(self, tree: ast.AST) -> List[Dict[str, Any]]:
        """Extract function definitions (module-level only)"""
        functions = []
        
        for node in tree.body:
            if isinstance(node, ast.FunctionDef):
                function_info = {
                    "name": node.name,
                    "line": node.lineno,
                    "column": node.col_offset,
                    "args": [arg.arg for arg in node.args.args],
                    "defaults": len(node.args.defaults),
                    "decorators": [self._ast_to_string(dec) for dec in node.decorator_list],
                    "docstring": ast.get_docstring(node) if self.parse_options.get("extractDocstrings", True) else None,
                    "returns": self._ast_to_string(node.returns) if node.returns else None
                }
                
                # Extract parameter types if available
                if node.args.args:
                    function_info["typed_args"] = []
                    for arg in node.args.args:
                        arg_info = {"name": arg.arg}
                        if arg.annotation:
                            arg_info["type"] = self._ast_to_string(arg.annotation)
                        function_info["typed_args"].append(arg_info)
                
                functions.append(function_info)
        
        return functions
    
    def _extract_variables(self, tree: ast.AST) -> List[Dict[str, Any]]:
        """Extract variable assignments (module-level only)"""
        variables = []
        
        for node in tree.body:
            if isinstance(node, ast.Assign):
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        var_info = {
                            "name": target.id,
                            "line": node.lineno,
                            "type": "assignment",
                            "value_type": type(node.value).__name__
                        }
                        
                        # Try to extract literal values
                        if isinstance(node.value, (ast.Constant, ast.Str, ast.Num)):
                            try:
                                var_info["value"] = ast.literal_eval(node.value)
                            except (ValueError, TypeError):
                                pass
                        
                        variables.append(var_info)
        
        return variables
    
    def _extract_positions(self, tree: ast.AST) -> Dict[str, Any]:
        """Extract position information for all nodes"""
        positions = {}
        
        for node in ast.walk(tree):
            if hasattr(node, 'lineno') and hasattr(node, 'col_offset'):
                node_id = f"{type(node).__name__}_{node.lineno}_{node.col_offset}"
                positions[node_id] = {
                    "line": node.lineno,
                    "column": node.col_offset,
                    "end_line": getattr(node, 'end_lineno', None),
                    "end_column": getattr(node, 'end_col_offset', None)
                }
        
        return positions
    
    def _ast_to_string(self, node: ast.AST) -> str:
        """Convert AST node to string representation"""
        try:
            import astor  # Optional dependency for better AST string conversion
            return astor.to_source(node).strip()
        except ImportError:
            # Fallback to simple string representation
            if isinstance(node, ast.Name):
                return node.id
            elif isinstance(node, ast.Constant):
                return str(node.value)
            elif isinstance(node, ast.Attribute):
                return f"{self._ast_to_string(node.value)}.{node.attr}"
            else:
                return type(node).__name__
    
    def _create_basic_ast(self, content: str, file_path: str, language: str) -> Dict[str, Any]:
        """Create basic AST information for unsupported languages"""
        lines = content.splitlines()
        
        # Simple pattern-based extraction for basic information
        basic_info = {
            "type": "BasicAST",
            "language": language,
            "line_count": len(lines),
            "estimated_functions": len([line for line in lines if 'function' in line.lower() or 'def ' in line]),
            "estimated_classes": len([line for line in lines if 'class ' in line.lower()]),
            "comments": len([line for line in lines if line.strip().startswith(('//', '#', '/*'))]),
            "blank_lines": len([line for line in lines if not line.strip()])
        }
        
        return basic_info
    
    def _get_detected_languages(self, parsed_results: List[Dict[str, Any]]) -> List[str]:
        """Get list of detected languages from parsed results"""
        languages = set()
        for result in parsed_results:
            lang = result.get("metadata", {}).get("language")
            if lang:
                languages.add(lang)
        return list(languages)
    
    def _calculate_parsing_statistics(self, parsed_results: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Calculate comprehensive parsing statistics"""
        if not parsed_results:
            return {}
        
        total_size = sum(r.get("metadata", {}).get("fileSize", 0) for r in parsed_results)
        total_lines = sum(r.get("metadata", {}).get("lineCount", 0) for r in parsed_results)
        total_functions = sum(len(r.get("ast", {}).get("functions", [])) for r in parsed_results)
        total_classes = sum(len(r.get("ast", {}).get("classes", [])) for r in parsed_results)
        
        return {
            "total_size_bytes": total_size,
            "total_lines": total_lines,
            "total_functions": total_functions,
            "total_classes": total_classes,
            "average_file_size": total_size / len(parsed_results),
            "average_lines_per_file": total_lines / len(parsed_results),
            "files_with_syntax_errors": len([r for r in parsed_results if r.get("ast", {}).get("type") == "SyntaxError"])
        }