#!/usr/bin/env python3
"""
Script to update COBOL entity descriptions with business-focused descriptions
"""
import sys
import os
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

from graph_manager import CodeGraphManager
import requests
import json

def generate_business_description(entity_name, entity_type, code_snippet=""):
    """Generate business-focused description using Ollama"""
    try:
        # Create business-focused prompt for COBOL entities
        prompt = f"""Analyze this COBOL banking system component and describe its specific business logic:

Entity: {entity_name}
Type: {entity_type}

Code Context:
{code_snippet[:500] if code_snippet else 'No code snippet available'}

Business Context: Banking system for account management, interest calculation, and reporting

Based on the entity name and context, describe the SPECIFIC business function this component performs in the banking system. Focus on:
- What specific banking operation it handles
- What data it processes
- What business rules it implements
- What output it produces

Provide a concise, specific description in 1-2 sentences that explains the actual business purpose, not generic technical details."""
        
        # Call Ollama API
        response = requests.post(
            "http://localhost:11434/api/generate",
            json={
                "model": "tinyllama:latest",
                "prompt": prompt,
                "stream": False
            },
            timeout=30
        )
        
        if response.status_code == 200:
            result = response.json()
            description = result.get("response", "").strip()
            
            # Clean up the description
            if description:
                # Remove any quotes if present
                if description.startswith('"') and description.endswith('"'):
                    description = description[1:-1]
                
                # Take only the first 1-2 sentences for conciseness
                sentences = description.split('. ')
                if len(sentences) > 2:
                    description = '. '.join(sentences[:2]) + '.'
                
                return description
        
        return f"A {entity_type} named {entity_name}"
        
    except Exception as e:
        print(f"Error generating description for {entity_name}: {e}")
        return f"A {entity_type} named {entity_name}"

def main():
    """Main function to update descriptions with business focus"""
    print("🔧 Updating COBOL entity descriptions with business-focused content...")
    
    # Connect to graph database
    gm = CodeGraphManager()
    
    try:
        # Get COBOL entities that need business-focused descriptions
        result = gm.graph.run("""
            MATCH (n) 
            WHERE n.name IS NOT NULL 
            AND (n.name CONTAINS 'BANKING' OR n.name CONTAINS 'ACCOUNT' OR n.name CONTAINS 'INTEREST' 
                 OR n.name CONTAINS 'DEPOSIT' OR n.name CONTAINS 'WITHDRAWAL' OR n.name CONTAINS 'TRANSFER'
                 OR n.name CONTAINS 'VALIDATE' OR n.name CONTAINS 'PROCESS' OR n.name CONTAINS 'GENERATE'
                 OR n.name CONTAINS 'CREATE' OR n.name CONTAINS 'UPDATE' OR n.name CONTAINS 'DISPLAY'
                 OR n.name CONTAINS 'INITIALIZE' OR n.name CONTAINS 'CLEANUP' OR n.name CONTAINS 'CLOSE'
                 OR n.name CONTAINS 'MAIN' OR n.name CONTAINS 'MENU' OR n.name CONTAINS 'REPORT'
                 OR n.name CONTAINS 'CUSTOMER' OR n.name CONTAINS 'TRANSACTION' OR n.name CONTAINS 'FUNDS'
                 OR n.name CONTAINS 'BALANCE' OR n.name CONTAINS 'RECORD' OR n.name CONTAINS 'DATA'
                 OR n.name CONTAINS 'FILE' OR n.name CONTAINS 'INQUIRY' OR n.name CONTAINS 'SUMMARY'
                 OR n.name CONTAINS 'DETAIL' OR n.name CONTAINS 'PAGE' OR n.name CONTAINS 'LINE'
                 OR n.name CONTAINS 'AMOUNT' OR n.name CONTAINS 'INFO' OR n.name CONTAINS 'CHOICE'
                 OR n.name CONTAINS 'SUFFICIENT' OR n.name CONTAINS 'LOOKUP' OR n.name CONTAINS 'WRITE'
                 OR n.name CONTAINS 'READ' OR n.name CONTAINS 'GET' OR n.name CONTAINS 'CHECK'
                 OR n.name CONTAINS 'END' OR n.name CONTAINS 'LOGIC' OR n.name CONTAINS 'INPUT')
            RETURN n.name, n.type, n.description
            ORDER BY n.name
        """)
        
        entities = list(result)
        print(f"Found {len(entities)} COBOL entities to update")
        
        updated_count = 0
        for record in entities:
            entity_name = record["n.name"]
            entity_type = record["n.type"] or "entity"
            current_description = record["n.description"]
            
            print(f"Updating {entity_name} ({entity_type})...")
            
            # Determine entity type from name if type is None
            if entity_type == "entity":
                if "UNKNOWN" in entity_name:
                    entity_type = "program"
                elif any(x in entity_name for x in ["Banking_system", "Test_cobol_banking", "Account_management"]):
                    entity_type = "compilation_unit"
                elif entity_name.endswith(".cbl"):
                    entity_type = "file"
                else:
                    entity_type = "program"
            
            # Generate new business-focused description
            new_description = generate_business_description(entity_name, entity_type, "")
            
            # Update the entity in the graph
            gm.graph.run("""
                MATCH (n {name: $name})
                SET n.description = $description, n.ai_description = $description
            """, name=entity_name, description=new_description)
            
            print(f"  ✅ Updated: {new_description}")
            updated_count += 1
        
        print(f"\n✅ Successfully updated {updated_count} entity descriptions with business-focused content!")
        
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        gm.close()

if __name__ == "__main__":
    main()
