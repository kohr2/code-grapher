"""
Multi-Language Parser Service

Provides AST parsing support for multiple programming languages using tree-sitter.
Supports Python (AST), TypeScript, JavaScript, and can be extended for other languages.
"""

import os
import ast
import json
import re
from typing import Dict, List, Any, Optional
from pathlib import Path

try:
    import tree_sitter_python
    import tree_sitter_typescript
    import tree_sitter_javascript
    from tree_sitter import Language, Parser

    TREE_SITTER_AVAILABLE = True
except ImportError as e:
    print(f"Tree-sitter not available: {e}")
    TREE_SITTER_AVAILABLE = False

# Import COBOL parser
try:
    import sys
    import os
    # Add cobol-support directory to path
    cobol_support_path = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(__file__))), 'cobol-support')
    if cobol_support_path not in sys.path:
        sys.path.insert(0, cobol_support_path)
    from services.cobol_parser import COBOLParser
    COBOL_PARSER_AVAILABLE = True
except ImportError:
    COBOL_PARSER_AVAILABLE = False
    print("⚠️  COBOL parser not available")


class MultiLanguageParser:
    """Parser that handles multiple programming languages including COBOL"""

    def __init__(self):
        self.parsers = {}
        self.languages = {
            "python": "Python AST Parser",
            "typescript": "Tree-sitter TypeScript Parser", 
            "javascript": "Tree-sitter JavaScript Parser"
        }

        if TREE_SITTER_AVAILABLE:
            self._setup_tree_sitter_parsers()

        # Add COBOL parser if available
        if COBOL_PARSER_AVAILABLE:
            self.cobol_parser = COBOLParser()
            self.languages["cobol"] = "Simple COBOL Parser"

    def _setup_tree_sitter_parsers(self):
        """Setup tree-sitter parsers for supported languages"""
        try:
            # Python - use new API with direct language in constructor
            self.parsers["python"] = Parser(tree_sitter_python.language())

            # TypeScript
            self.parsers["typescript"] = Parser(tree_sitter_typescript.language_typescript())

            # JavaScript
            self.parsers["javascript"] = Parser(tree_sitter_javascript.language())

            print("Tree-sitter parsers initialized successfully")

        except Exception as e:
            print(f"Warning: Failed to setup tree-sitter parsers: {e}")
            print("Falling back to Python AST only")
            import traceback

            traceback.print_exc()

    def get_language_from_extension(self, file_path: str) -> str:
        """Get language from file extension"""
        ext = Path(file_path).suffix.lower()
        
        extension_map = {
            ".py": "python",
            ".ts": "typescript",
            ".js": "javascript",
            ".md": "markdown",
            ".markdown": "markdown",
        }
        
        # Add COBOL file extensions if parser is available
        if COBOL_PARSER_AVAILABLE:
            extension_map.update({
                ".cbl": "cobol",
                ".cob": "cobol", 
                ".cobol": "cobol"
            })

        return extension_map.get(ext, "unknown")

    def parse_file(self, file_path: str) -> Dict[str, Any]:
        """Parse file with appropriate parser"""
        language = self.get_language_from_extension(file_path)
        
        # Handle COBOL files
        if language == 'cobol' and COBOL_PARSER_AVAILABLE:
            return self.cobol_parser.parse_file(file_path)
        
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            if language == "python":
                return self._parse_python_ast(file_path, content)
            elif language == "typescript":
                return self._parse_typescript(file_path, content)
            elif language == "javascript":
                return self._parse_javascript(file_path, content)
            elif language == "markdown":
                return self._parse_markdown(file_path, content)
            else:
                # Fallback: basic file info without parsing
                return {
                    "parse_success": False,
                    "language": language,
                    "file_path": file_path,
                    "entities": [],
                    "content": content,
                    "error": f"No parser available for {language}"
                }

        except Exception as e:
            return {
                "parse_success": False,
                "language": language,
                "file_path": file_path,
                "entities": [],
                "error": str(e)
            }

    def _parse_python_ast(self, file_path: str, content: str) -> Dict[str, Any]:
        """Parse Python file using AST"""
        try:
            tree = ast.parse(content)
            entities = []

            # Extract functions, classes, and imports
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    # Extract parameter info
                    param_info = []
                    for arg in node.args.args:
                        param_data = {"name": arg.arg}
                        if arg.annotation:
                            param_data["type"] = (
                                ast.unparse(arg.annotation) if hasattr(ast, "unparse") else str(arg.annotation)
                            )
                        param_info.append(param_data)

                    # Extract return type
                    return_type = None
                    if node.returns:
                        return_type = ast.unparse(node.returns) if hasattr(ast, "unparse") else str(node.returns)

                    # Extract decorators
                    decorators = []
                    for decorator in node.decorator_list:
                        if isinstance(decorator, ast.Name):
                            decorators.append(decorator.id)
                        elif isinstance(decorator, ast.Call) and isinstance(decorator.func, ast.Name):
                            decorators.append(decorator.func.id)

                    entities.append(
                        {
                            "type": "function",
                            "name": node.name,
                            "line_number": node.lineno,
                            "parameters": param_info,
                            "return_type": return_type,
                            "decorators": decorators,
                            "docstring": ast.get_docstring(node),
                        }
                    )

                elif isinstance(node, ast.ClassDef):
                    # Extract base classes
                    bases = []
                    for base in node.bases:
                        if isinstance(base, ast.Name):
                            bases.append(base.id)
                        elif isinstance(base, ast.Attribute):
                            bases.append(
                                f"{base.value.id}.{base.attr}" if isinstance(base.value, ast.Name) else str(base)
                            )

                    # Extract decorators
                    decorators = []
                    for decorator in node.decorator_list:
                        if isinstance(decorator, ast.Name):
                            decorators.append(decorator.id)
                        elif isinstance(decorator, ast.Call) and isinstance(decorator.func, ast.Name):
                            decorators.append(decorator.func.id)

                    entities.append(
                        {
                            "type": "class",
                            "name": node.name,
                            "line_number": node.lineno,
                            "bases": bases,
                            "decorators": decorators,
                            "docstring": ast.get_docstring(node),
                        }
                    )

                elif isinstance(node, ast.Import):
                    for alias in node.names:
                        entities.append(
                            {"type": "import", "name": alias.name, "alias": alias.asname, "line_number": node.lineno}
                        )

                elif isinstance(node, ast.ImportFrom):
                    for alias in node.names:
                        entities.append(
                            {
                                "type": "import_from",
                                "module": node.module,
                                "name": alias.name,
                                "alias": alias.asname,
                                "line_number": node.lineno,
                            }
                        )

            return {
                "file_path": file_path,
                "entities": entities,
                "language": "python",
                "lines_of_code": len(content.splitlines()),
                "parse_success": True,
                "error": None,
            }

        except Exception as e:
            return {
                "file_path": file_path,
                "entities": [],
                "language": "python",
                "lines_of_code": len(content.splitlines()) if content else 0,
                "parse_success": False,
                "error": str(e),
            }

    def _parse_typescript_javascript(self, file_path: str, content: str, language: str) -> Dict[str, Any]:
        """Parse TypeScript/JavaScript file using our advanced ts-morph extractor"""
        try:
            # Import our TypeScript extractor service
            import sys
            import os

            project_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
            if project_root not in sys.path:
                sys.path.insert(0, project_root)

            from ts_relationship_extractor_service import TSRelationshipExtractorService
            from js_entity_classifier import classify_js_entities

            # Create extractor instance
            project_dir = os.path.dirname(file_path)
            extractor = TSRelationshipExtractorService(project_dir)

            # Analyze the single file
            analysis_result = extractor.analyze_file(file_path, project_dir)

            if analysis_result.get("success", False):
                entities = analysis_result.get("entities", [])

                # Enhance entities with classification
                file_context = {"file_path": file_path, "project_root": project_dir}
                enhanced_entities = classify_js_entities(entities, file_context)

                return {
                    "file_path": file_path,
                    "entities": enhanced_entities,
                    "language": language,
                    "lines_of_code": len(content.splitlines()),
                    "parse_success": True,
                    "error": None,
                    "ts_morph_analysis": True,  # Flag to indicate advanced analysis
                }
            else:
                # Fallback to pattern-based parsing if ts-morph fails
                print(
                    f"⚠️ ts-morph analysis failed for {file_path}, using fallback: {analysis_result.get('error', 'Unknown error')}"
                )
                return self._parse_ts_js_fallback(file_path, content, language)

        except Exception as e:
            print(f"⚠️ TypeScript extractor failed for {file_path}: {e}")
            # Fallback to pattern-based parsing
            return self._parse_ts_js_fallback(file_path, content, language)

    def _parse_with_tree_sitter(self, file_path: str, content: str, language: str) -> Dict[str, Any]:
        """Parse TypeScript/JavaScript file using tree-sitter or fallback pattern matching"""
        try:
            if language in self.parsers:
                # Use tree-sitter if available
                parser = self.parsers[language]
                tree = parser.parse(content.encode("utf-8"))
                root_node = tree.root_node

                entities = []
                self._extract_ts_js_entities(root_node, content, entities)

                return {
                    "file_path": file_path,
                    "entities": entities,
                    "language": language,
                    "lines_of_code": len(content.splitlines()),
                    "parse_success": True,
                    "error": None,
                }
            else:
                # Fallback to pattern-based parsing
                return self._parse_ts_js_fallback(file_path, content, language)

        except Exception as e:
            # If tree-sitter fails, try fallback
            return self._parse_ts_js_fallback(file_path, content, language)

    def _extract_ts_js_entities(self, node, content: str, entities: List[Dict[str, Any]]):
        """Extract entities from TypeScript/JavaScript AST"""
        content_lines = content.splitlines()

        def get_text(node):
            """Get text content of a node"""
            start_byte = node.start_byte
            end_byte = node.end_byte
            return content[start_byte:end_byte]

        # Function declarations
        if node.type in ["function_declaration", "method_definition", "arrow_function"]:
            function_name = None
            parameters = []
            return_type = None

            for child in node.children:
                if child.type == "identifier" and function_name is None:
                    function_name = get_text(child)
                elif child.type == "formal_parameters":
                    for param_child in child.children:
                        if param_child.type in ["identifier", "required_parameter", "optional_parameter"]:
                            param_name = get_text(param_child)
                            if param_name not in ["(", ")", ","]:
                                parameters.append({"name": param_name})
                elif child.type == "type_annotation":
                    return_type = get_text(child)

            if function_name:
                entities.append(
                    {
                        "type": "function",
                        "name": function_name,
                        "line_number": node.start_point[0] + 1,
                        "parameters": parameters,
                        "return_type": return_type,
                        "decorators": [],
                        "docstring": None,
                    }
                )

        # Class declarations
        elif node.type == "class_declaration":
            class_name = None
            bases = []

            for child in node.children:
                if child.type == "type_identifier" and class_name is None:
                    class_name = get_text(child)
                elif child.type == "class_heritage":
                    for heritage_child in child.children:
                        if heritage_child.type == "extends_clause":
                            for extends_child in heritage_child.children:
                                if extends_child.type == "identifier":
                                    bases.append(get_text(extends_child))

            if class_name:
                entities.append(
                    {
                        "type": "class",
                        "name": class_name,
                        "line_number": node.start_point[0] + 1,
                        "bases": bases,
                        "decorators": [],
                        "docstring": None,
                    }
                )

        # Interface declarations (TypeScript)
        elif node.type == "interface_declaration":
            interface_name = None

            for child in node.children:
                if child.type == "type_identifier" and interface_name is None:
                    interface_name = get_text(child)

            if interface_name:
                entities.append(
                    {
                        "type": "interface",
                        "name": interface_name,
                        "line_number": node.start_point[0] + 1,
                        "bases": [],
                        "decorators": [],
                        "docstring": None,
                    }
                )

        # Import statements
        elif node.type == "import_statement":
            import_clause = None
            from_clause = None

            for child in node.children:
                if child.type == "import_clause":
                    import_clause = get_text(child)
                elif child.type == "string":
                    from_clause = get_text(child).strip("\"'")

            if import_clause and from_clause:
                entities.append(
                    {
                        "type": "import",
                        "name": import_clause,
                        "module": from_clause,
                        "line_number": node.start_point[0] + 1,
                    }
                )

        # Recursively process children
        for child in node.children:
            self._extract_ts_js_entities(child, content, entities)

    def _parse_ts_js_fallback(self, file_path: str, content: str, language: str) -> Dict[str, Any]:
        """Fallback pattern-based parsing for TypeScript/JavaScript"""
        try:
            entities = []
            lines = content.splitlines()

            for line_num, line in enumerate(lines, 1):
                line = line.strip()

                # Interface declarations
                if line.startswith("interface "):
                    match = line.split()
                    if len(match) >= 2:
                        interface_name = match[1].rstrip("{")
                        entities.append(
                            {
                                "type": "interface",
                                "name": interface_name,
                                "line_number": line_num,
                                "bases": [],
                                "decorators": [],
                                "docstring": None,
                            }
                        )

                # Class declarations
                elif line.startswith("class "):
                    match = line.split()
                    if len(match) >= 2:
                        class_name = match[1].rstrip("{")
                        bases = []
                        if "extends" in line:
                            extends_part = line.split("extends")[1].split("{")[0].strip()
                            bases.append(extends_part)

                        entities.append(
                            {
                                "type": "class",
                                "name": class_name,
                                "line_number": line_num,
                                "bases": bases,
                                "decorators": [],
                                "docstring": None,
                            }
                        )

                # Function declarations
                elif ("function " in line or "=>" in line) and not line.startswith("//"):
                    function_name = None
                    parameters = []

                    if line.startswith("function "):
                        # Traditional function
                        parts = line.split("(")
                        if len(parts) >= 2:
                            function_name = parts[0].replace("function", "").strip()
                            param_part = parts[1].split(")")[0] if ")" in parts[1] else ""
                            if param_part:
                                parameters = [
                                    {"name": p.strip().split(":")[0]} for p in param_part.split(",") if p.strip()
                                ]

                    elif "async " in line and "=>" in line:
                        # Async arrow function
                        if "=" in line:
                            function_name = line.split("=")[0].replace("async", "").strip()

                    elif "=>" in line and "=" in line:
                        # Arrow function
                        function_name = line.split("=")[0].strip()

                    elif line.strip().endswith("): ") or line.strip().endswith(") {"):
                        # Method declaration
                        parts = line.split("(")
                        if len(parts) >= 2:
                            function_name_part = parts[0].strip()
                            # Remove access modifiers
                            for modifier in ["private", "public", "protected", "static", "async"]:
                                function_name_part = function_name_part.replace(modifier, "").strip()
                            function_name = function_name_part

                            param_part = parts[1].split(")")[0] if ")" in parts[1] else ""
                            if param_part:
                                parameters = [
                                    {"name": p.strip().split(":")[0]} for p in param_part.split(",") if p.strip()
                                ]

                    if function_name:
                        entities.append(
                            {
                                "type": "function",
                                "name": function_name,
                                "line_number": line_num,
                                "parameters": parameters,
                                "return_type": None,
                                "decorators": [],
                                "docstring": None,
                            }
                        )

                # Import statements
                elif line.startswith("import "):
                    if "from" in line:
                        # import { ... } from '...'
                        parts = line.split("from")
                        if len(parts) >= 2:
                            module = parts[1].strip().strip("'\"")
                            import_part = parts[0].replace("import", "").strip()
                            entities.append(
                                {"type": "import", "name": import_part, "module": module, "line_number": line_num}
                            )
                    else:
                        # import ...
                        import_name = line.replace("import", "").strip().rstrip(";")
                        entities.append(
                            {"type": "import", "name": import_name, "module": None, "line_number": line_num}
                        )

            return {
                "file_path": file_path,
                "entities": entities,
                "language": language,
                "lines_of_code": len(lines),
                "parse_success": True,
                "error": None,
            }

        except Exception as e:
            return {
                "file_path": file_path,
                "entities": [],
                "language": language,
                "lines_of_code": len(content.splitlines()) if content else 0,
                "parse_success": False,
                "error": str(e),
            }

    def _parse_json(self, file_path: str, content: str) -> Dict[str, Any]:
        """Parse JSON file and extract entities"""
        try:
            data = json.loads(content)
            entities = []
            filename = Path(file_path).name.lower()

            # Handle package.json
            if filename == "package.json":
                entities.extend(self._extract_package_json_entities(data))

            # Handle tsconfig.json
            elif filename == "tsconfig.json":
                entities.extend(self._extract_tsconfig_entities(data))

            # Handle generic JSON configuration files
            elif any(keyword in filename for keyword in ["config", "settings", ".eslintrc", ".prettierrc"]):
                entities.extend(self._extract_config_entities(data, filename))

            # Handle other JSON files as data structures
            else:
                entities.extend(self._extract_generic_json_entities(data))

            return {
                "file_path": file_path,
                "entities": entities,
                "language": "json",
                "lines_of_code": len(content.splitlines()),
                "parse_success": True,
                "error": None,
            }

        except json.JSONDecodeError as e:
            return {
                "file_path": file_path,
                "entities": [],
                "language": "json",
                "lines_of_code": len(content.splitlines()) if content else 0,
                "parse_success": False,
                "error": f"Invalid JSON: {str(e)}",
            }
        except Exception as e:
            return {
                "file_path": file_path,
                "entities": [],
                "language": "json",
                "lines_of_code": len(content.splitlines()) if content else 0,
                "parse_success": False,
                "error": str(e),
            }

    def _extract_package_json_entities(self, data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract entities from package.json"""
        entities = []

        # Project metadata
        if "name" in data:
            entities.append(
                {
                    "type": "project",
                    "name": data["name"],
                    "line_number": 1,
                    "metadata": {
                        "version": data.get("version"),
                        "description": data.get("description"),
                        "main": data.get("main"),
                    },
                }
            )

        # Dependencies
        for dep_type in ["dependencies", "devDependencies", "peerDependencies"]:
            if dep_type in data:
                for name, version in data[dep_type].items():
                    entities.append(
                        {
                            "type": "dependency",
                            "name": name,
                            "line_number": 1,
                            "dependency_type": dep_type,
                            "version": version,
                        }
                    )

        # Scripts
        if "scripts" in data:
            for name, command in data["scripts"].items():
                entities.append({"type": "script", "name": name, "line_number": 1, "command": command})

        return entities

    def _extract_tsconfig_entities(self, data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract entities from tsconfig.json"""
        entities = []

        # Compiler options
        if "compilerOptions" in data:
            for option, value in data["compilerOptions"].items():
                entities.append(
                    {
                        "type": "config_option",
                        "name": f"compilerOptions.{option}",
                        "line_number": 1,
                        "value": value,
                        "category": "typescript_compiler",
                    }
                )

        # Include/exclude patterns
        for pattern_type in ["include", "exclude", "files"]:
            if pattern_type in data:
                entities.append(
                    {
                        "type": "file_pattern",
                        "name": pattern_type,
                        "line_number": 1,
                        "patterns": (
                            data[pattern_type] if isinstance(data[pattern_type], list) else [data[pattern_type]]
                        ),
                    }
                )

        return entities

    def _extract_config_entities(self, data: Dict[str, Any], filename: str) -> List[Dict[str, Any]]:
        """Extract entities from configuration files"""
        entities = []

        def extract_config_recursive(obj, prefix=""):
            for key, value in obj.items():
                full_key = f"{prefix}.{key}" if prefix else key

                if isinstance(value, dict):
                    extract_config_recursive(value, full_key)
                else:
                    entities.append(
                        {
                            "type": "config_option",
                            "name": full_key,
                            "line_number": 1,
                            "value": value,
                            "category": filename.replace(".json", "").replace(".", "_"),
                        }
                    )

        if isinstance(data, dict):
            extract_config_recursive(data)

        return entities

    def _extract_generic_json_entities(self, data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract entities from generic JSON files"""
        entities = []

        # For generic JSON, create entities for top-level keys
        if isinstance(data, dict):
            for key, value in data.items():
                entities.append(
                    {
                        "type": "data_structure",
                        "name": key,
                        "line_number": 1,
                        "data_type": type(value).__name__,
                        "has_nested_structure": isinstance(value, (dict, list)),
                    }
                )

        return entities

    def _parse_markdown(self, file_path: str, content: str) -> Dict[str, Any]:
        """Parse Markdown file and extract entities"""
        try:
            entities = []
            lines = content.splitlines()
            filename = Path(file_path).name.lower()

            current_section = None
            code_block_language = None
            in_code_block = False

            for line_num, line in enumerate(lines, 1):
                line = line.strip()

                # Headers
                if line.startswith("#"):
                    header_level = len(line) - len(line.lstrip("#"))
                    header_text = line.lstrip("# ").strip()

                    if header_text:
                        entities.append(
                            {
                                "type": "documentation_section",
                                "name": header_text,
                                "line_number": line_num,
                                "level": header_level,
                                "section_type": "header",
                            }
                        )
                        current_section = header_text

                # Code blocks
                elif line.startswith("```"):
                    if not in_code_block:
                        # Starting code block
                        in_code_block = True
                        code_block_language = line[3:].strip() or "text"

                        entities.append(
                            {
                                "type": "code_example",
                                "name": f"code_block_{code_block_language}",
                                "line_number": line_num,
                                "language": code_block_language,
                                "section": current_section,
                            }
                        )
                    else:
                        # Ending code block
                        in_code_block = False
                        code_block_language = None

                # Links
                elif "[" in line and "](" in line:
                    # Extract markdown links
                    link_pattern = r"\[([^\]]+)\]\(([^)]+)\)"
                    matches = re.findall(link_pattern, line)

                    for link_text, link_url in matches:
                        entities.append(
                            {
                                "type": "reference_link",
                                "name": link_text,
                                "line_number": line_num,
                                "url": link_url,
                                "section": current_section,
                            }
                        )

                # API references (lines that look like function/class references)
                elif (
                    line
                    and not line.startswith("#")
                    and ("()" in line or "class " in line.lower() or "interface " in line.lower())
                ):
                    # Extract potential API references
                    api_matches = re.findall(r"`([^`]+)`", line)
                    for match in api_matches:
                        if any(keyword in match.lower() for keyword in ["function", "class", "interface", "method"]):
                            entities.append(
                                {
                                    "type": "api_reference",
                                    "name": match,
                                    "line_number": line_num,
                                    "section": current_section,
                                }
                            )

            # Special handling for README files
            if filename.startswith("readme"):
                entities.insert(
                    0, {"type": "project_documentation", "name": "README", "line_number": 1, "is_main_readme": True}
                )

            return {
                "file_path": file_path,
                "entities": entities,
                "language": "markdown",
                "lines_of_code": len(lines),
                "parse_success": True,
                "error": None,
            }

        except Exception as e:
            return {
                "file_path": file_path,
                "entities": [],
                "language": "markdown",
                "lines_of_code": len(content.splitlines()) if content else 0,
                "parse_success": False,
                "error": str(e),
            }


def parse_and_extract_entities(file_paths: List[str]) -> List[Dict[str, Any]]:
    """Parse multiple files and extract entities - new multi-language version"""
    print(f"🔍 Parsing {len(file_paths)} files with multi-language support...")

    parser = MultiLanguageParser()
    parsed_files = []

    for file_path in file_paths:
        print(f"   📄 Reading: {file_path}")
        result = parser.parse_file(file_path)
        parsed_files.append(result)

        if result["parse_success"]:
            entity_count = len(result["entities"])
            print(f"   ✅ Parsed {entity_count} entities from {result['language']} file")
        else:
            print(f"   ❌ Failed to parse {file_path}: {result['error']}")

    return parsed_files


def extract_multi_language_relationships(parsed_files: List[Dict[str, Any]]) -> List[Any]:
    """Extract relationships from multi-language parsed files"""
    print(f"🔗 Extracting relationships from {len(parsed_files)} parsed files...")

    all_relationships = []

    # Separate files by language for specialized relationship extraction
    python_files = []
    ts_js_files = []
    cobol_files = []
    other_files = []

    for file_data in parsed_files:
        if not file_data.get("parse_success", False):
            continue

        language = file_data.get("language", "").lower()
        if language == "python":
            python_files.append(file_data)
        elif language in ["typescript", "javascript"]:
            ts_js_files.append(file_data)
        elif language == "cobol":
            cobol_files.append(file_data)
        else:
            other_files.append(file_data)

    # Extract Python relationships using AST extractor
    if python_files:
        print(f"   🐍 Extracting Python relationships from {len(python_files)} files...")
        try:
            from ast_relationship_extractor import extract_ast_relationships

            python_relationships = extract_ast_relationships(python_files)
            all_relationships.extend(python_relationships)
            print(f"   ✅ Extracted {len(python_relationships)} Python relationships")
        except Exception as e:
            print(f"   ❌ Failed to extract Python relationships: {e}")

    # Extract TypeScript/JavaScript relationships using ts-morph extractor
    if ts_js_files:
        print(f"   🟨 Extracting TypeScript/JavaScript relationships from {len(ts_js_files)} files...")
        try:
            # Get file paths for TypeScript extractor
            file_paths = [f["file_path"] for f in ts_js_files]
            project_root = os.path.commonpath(file_paths) if len(file_paths) > 1 else os.path.dirname(file_paths[0])

            from ts_relationship_extractor_service import TSRelationshipExtractorService

            extractor = TSRelationshipExtractorService(project_root)
            ts_relationships = extractor.extract_relationships(file_paths, project_root)

            # Convert JS relationships to compatible format
            from ai_relationship_extractor import RelationshipExtraction, RelationshipType

            # Map JS relationship types to our enum
            type_mapping = {
                "IMPORTS": RelationshipType.IMPORTS,
                "EXPORTS": RelationshipType.EXPORTS,
                "EXTENDS": RelationshipType.INHERITS,
                "IMPLEMENTS": RelationshipType.IMPLEMENTS,
                "DECORATES": RelationshipType.DECORATES,
                "CALLS": RelationshipType.CALLS,
                "TYPE_DEPENDENCY": RelationshipType.USES,
            }

            for js_rel in ts_relationships:
                try:
                    # Handle both enum objects and string values
                    js_rel_type = js_rel.relationship_type
                    if hasattr(js_rel_type, "value"):
                        js_rel_type_str = js_rel_type.value
                    else:
                        js_rel_type_str = str(js_rel_type)

                    relationship_type = type_mapping.get(js_rel_type_str, RelationshipType.USES)

                    compatible_rel = RelationshipExtraction(
                        source_file=js_rel.source_file,
                        target_file=js_rel.target_file,
                        source_entity=js_rel.source_entity,
                        target_entity=js_rel.target_entity,
                        relationship_type=relationship_type,
                        confidence=js_rel.confidence,
                        relationship_strength=js_rel.relationship_strength,
                        line_number=js_rel.line_number,
                        context=js_rel.context or "",
                    )
                    all_relationships.append(compatible_rel)
                except Exception as e:
                    print(
                        f"      ⚠️ Failed to convert relationship {js_rel.source_entity} -> {js_rel.target_entity}: {e}"
                    )
                    continue

            print(f"   ✅ Extracted {len(ts_relationships)} TypeScript/JavaScript relationships")
        except Exception as e:
            print(f"   ❌ Failed to extract TypeScript/JavaScript relationships: {e}")

    # Extract COBOL relationships
    if cobol_files:
        print(f"   🟦 Extracting COBOL relationships from {len(cobol_files)} files...")
        try:
            # Import COBOL relationship extractor with proper path
            import sys
            import os
            cobol_support_path = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(__file__))), 'cobol-support')
            if cobol_support_path not in sys.path:
                sys.path.insert(0, cobol_support_path)
            from cobol_relationship_extractor import extract_cobol_relationships
            
            for cobol_file in cobol_files:
                cobol_relationships = extract_cobol_relationships(cobol_file)
                all_relationships.extend(cobol_relationships)
                
            print(f"   ✅ Extracted {len(all_relationships)} COBOL relationships from {len(cobol_files)} files")
        except Exception as e:
            print(f"   ❌ Failed to extract COBOL relationships: {e}")
            import traceback
            traceback.print_exc()

    print(f"🎯 Total relationships extracted: {len(all_relationships)}")
    return all_relationships
