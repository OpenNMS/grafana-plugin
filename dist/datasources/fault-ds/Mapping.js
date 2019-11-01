'use strict';

System.register(['./mapping/ComparatorMapping', './mapping/OperatorMapping', './mapping/AttributeMapping', './mapping/FilterMapping', './mapping/RestrictionMapping', './mapping/ClauseMapping', './mapping/ValueMapping'], function (_export, _context) {
    "use strict";

    var ComparatorMapping, OperatorMapping, AttributeMapping, FilterMapping, RestrictionMapping, ClauseMapping, ValueMapping, Mapping;
    return {
        setters: [function (_mappingComparatorMapping) {
            ComparatorMapping = _mappingComparatorMapping.ComparatorMapping;
        }, function (_mappingOperatorMapping) {
            OperatorMapping = _mappingOperatorMapping.OperatorMapping;
        }, function (_mappingAttributeMapping) {
            AttributeMapping = _mappingAttributeMapping.AttributeMapping;
        }, function (_mappingFilterMapping) {
            FilterMapping = _mappingFilterMapping.FilterMapping;
        }, function (_mappingRestrictionMapping) {
            RestrictionMapping = _mappingRestrictionMapping.RestrictionMapping;
        }, function (_mappingClauseMapping) {
            ClauseMapping = _mappingClauseMapping.ClauseMapping;
        }, function (_mappingValueMapping) {
            ValueMapping = _mappingValueMapping.ValueMapping;
        }],
        execute: function () {
            _export('Mapping', Mapping = Object.freeze({
                ComparatorMapping: ComparatorMapping,
                OperatorMapping: OperatorMapping,
                AttributeMapping: AttributeMapping,
                FilterMapping: FilterMapping,
                RestrictionMapping: RestrictionMapping,
                ClauseMapping: ClauseMapping,
                ValueMapping: ValueMapping
            }));

            _export('Mapping', Mapping);
        }
    };
});
//# sourceMappingURL=Mapping.js.map
