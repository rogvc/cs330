using System;

namespace RudimentaryInterpreter
{
    internal abstract class Expression { }
    internal abstract class ExpressionWithTwoArguments
    {
        protected Expression LeftHandSide { get; private set; }
        protected Expression RightHandSide { get; private set; }

        protected string ToStringPrologue => this.GetType().ToString();

        public ExpressionWithTwoArguments(Expression leftHandSide, Expression rightHandSide)
        {
            LeftHandSide = leftHandSide;
            RightHandSide = rightHandSide;
        }
    }

    internal class Number : Expression
    {
        protected double Value { get; private set; }

        public Number(double value) => Value = value;

        public override string ToString() => Value.ToString();
    }

    internal class Symbol : Expression
    {
        protected string Value { get; private set; }

        public Symbol(string value) => Value = value;

        public override string ToString() => Value;
    }

    internal class Addition : ExpressionWithTwoArguments
    {
        public Addition(Expression leftHandSide, Expression rightHandSide)
            : base(leftHandSide, rightHandSide)
        {
        }

        public override string ToString()
            => String.Format(
                "{0}({1} {2})",
                ToStringPrologue,
                Interpreter.Parse(LeftHandSide.ToString()),
                Interpreter.Parse(RightHandSide.ToString())
                );
    }

    internal class Subtraction : ExpressionWithTwoArguments
    {
        public Subtraction(Expression leftHandSide, Expression rightHandSide)
            : base(leftHandSide, rightHandSide)
        {
        }

        public override string ToString()
            => String.Format(
                "{0}({1} {2})",
                ToStringPrologue,
                Interpreter.Parse(LeftHandSide.ToString()),
                Interpreter.Parse(RightHandSide.ToString())
                );
    }

    internal class Multiplication : ExpressionWithTwoArguments
    {
        public Multiplication(Expression leftHandSide, Expression rightHandSide)
            : base(leftHandSide, rightHandSide)
        {
        }

        public override string ToString()
            => String.Format(
                "{0}({1} {2})",
                ToStringPrologue,
                Interpreter.Parse(LeftHandSide.ToString()),
                Interpreter.Parse(RightHandSide.ToString())
                );
    }

    internal class Division : ExpressionWithTwoArguments
    {
        public Division(Expression leftHandSide, Expression rightHandSide)
            : base(leftHandSide, rightHandSide)
        {
        }

        public override string ToString()
            => String.Format(
                "{0}({1} {2})",
                ToStringPrologue,
                Interpreter.Parse(LeftHandSide.ToString()),
                Interpreter.Parse(RightHandSide.ToString())
                );
    }

    // internal class Substitution : Expression
    // {
    //     public Substitution(Expression leftHandSide, Expression rightHandSide)
    //         : base(leftHandSide, rightHandSide)
    //     {
    //     }

    //     public override string ToString()
    //         => String.Format(
    //             "Substitution({0} {1})",
    //             Interpreter.Parse(LeftHandSide.ToString()),
    //             Interpreter.Parse(RightHandSide.ToString())
    //             );
    // }

}