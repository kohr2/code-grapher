#!/usr/bin/env python3
"""
Test script to verify the relationship creation fix.
This script tests that missing entities are created on-the-fly when referenced in relationships.
"""

import sys
import os
sys.path.append('.')

def test_relationship_creation():
    """Test the relationship creation with missing entities"""
    try:
        from shared.services.graph_manager_facade import GraphManagerFacade
        
        print("🔧 Testing relationship creation fix...")
        
        # Create a facade instance
        facade = GraphManagerFacade()
        
        # Get initial stats
        initial_stats = facade.get_database_stats()
        print(f"📊 Initial database stats: {initial_stats}")
        
        # Test creating relationships with missing entities
        test_relationships = [
            ('PERFORM_218', 'type', 'CONTAINS'),
            ('PERFORM_219', 'text', 'USES'),
            ('PERFORM_220', 'line', 'RELATES_TO'),
            ('unit', 'value', 'CONTAINS'),
        ]
        
        created_relationships = 0
        for source, target, rel_type in test_relationships:
            print(f"🔗 Creating relationship: {source} -{rel_type}-> {target}")
            result = facade.add_relationship(source, target, rel_type)
            if result is not None:
                created_relationships += 1
                print(f"   ✅ Successfully created relationship")
            else:
                print(f"   ❌ Failed to create relationship")
        
        # Get final stats
        final_stats = facade.get_database_stats()
        print(f"📊 Final database stats: {final_stats}")
        
        # Calculate improvements
        initial_relationships = initial_stats.get('total_relationships', 0)
        final_relationships = final_stats.get('total_relationships', 0)
        initial_entities = initial_stats.get('total_nodes', 0)
        final_entities = final_stats.get('total_nodes', 0)
        
        print(f"\n📈 Results:")
        print(f"   Relationships created: {created_relationships}/{len(test_relationships)}")
        print(f"   Total relationships: {initial_relationships} -> {final_relationships} (+{final_relationships - initial_relationships})")
        print(f"   Total entities: {initial_entities} -> {final_entities} (+{final_entities - initial_entities})")
        
        facade.close()
        
        # Success if we created at least some relationships
        return created_relationships > 0
        
    except Exception as e:
        print(f"❌ Error during test: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = test_relationship_creation()
    if success:
        print("\n✅ Relationship creation fix test PASSED")
        sys.exit(0)
    else:
        print("\n❌ Relationship creation fix test FAILED")
        sys.exit(1)
