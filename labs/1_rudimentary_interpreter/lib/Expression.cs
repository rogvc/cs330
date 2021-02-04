using System;

namespace RudimentaryInterpreter
{
    internal abstract class Expression
    {
        internal abstract dynamic GetValue();
    }
    internal abstract class ExpressionWithTwoArguments : Expression
    {
        protected dynamic Value { get; private set; }

        protected Expression LeftHandSide { get; private set; }
        protected Expression RightHandSide { get; private set; }

        protected string operationName;

        public ExpressionWithTwoArguments(Expression leftHandSide, Expression rightHandSide)
        {
            LeftHandSide = leftHandSide;
            RightHandSide = rightHandSide;
        }

        public override string ToString()
        => String.Format(
            "{0}({1} {2})",
            operationName,
            LeftHandSide.ToString(),
            RightHandSide.ToString()
            );

        internal override dynamic GetValue() => Value;
    }

    internal class Number : Expression
    {
        protected double Value { get; private set; }

        public Number(double value) => Value = value;

        public override string ToString() => Value.ToString();

        internal override dynamic GetValue() => Value;
    }

    internal class Symbol : Expression
    {
        protected string Value { get; private set; }

        public Symbol(string value) => Value = value;

        public override string ToString() => Value;

        internal override dynamic GetValue() => Value;
    }

    internal class Addition : ExpressionWithTwoArguments
    {
        public Addition(Expression leftHandSide, Expression rightHandSide)
            : base(leftHandSide, rightHandSide)
        {
            operationName = "Addition";
        }
    }

    internal class Subtraction : ExpressionWithTwoArguments
    {
        public Subtraction(Expression leftHandSide, Expression rightHandSide)
            : base(leftHandSide, rightHandSide)
        {
            operationName = "Subtraction";
        }
    }

    internal class Multiplication : ExpressionWithTwoArguments
    {
        public Multiplication(Expression leftHandSide, Expression rightHandSide)
            : base(leftHandSide, rightHandSide)
        {
            operationName = "Multiplication";
        }
    }

    internal class Division : ExpressionWithTwoArguments
    {
        public Division(Expression leftHandSide, Expression rightHandSide)
            : base(leftHandSide, rightHandSide)
        {
            operationName = "Division";
        }
    }

    internal class Substitution : ExpressionWithTwoArguments
    {
        public Substitution(Expression leftHandSide, Expression rightHandSide)
            : base(leftHandSide, rightHandSide)
        {
            operationName = "Substitution";
        }
    }

}