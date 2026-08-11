{$mode objfpc}
unit SetInParenthesizedConst;

interface

type
    TModeFeature = (fA, fB);
    TModeFeatures = set of TModeFeature;

const
    Features: array[1..2] of TModeFeatures = (
        [],
        [fA, fB]
    );

implementation

end.
