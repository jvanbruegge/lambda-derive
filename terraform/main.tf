output "base_url" {
    value = aws_api_gateway_deployment.api_deployment.invoke_url
}

variable "telegram_bot_token" {
    type = string
}

provider "aws" {
    region = "eu-central-1"
}

# API gateway
resource "aws_api_gateway_rest_api" "api" {
    name = "Telegram Derivative Bot API"
}

resource "aws_api_gateway_resource" "api_resource" {
    rest_api_id = aws_api_gateway_rest_api.api.id
    parent_id = aws_api_gateway_rest_api.api.root_resource_id
    path_part = "{proxy+}"
}

resource "aws_api_gateway_method" "proxy_method" {
    rest_api_id = aws_api_gateway_rest_api.api.id
    resource_id = aws_api_gateway_resource.api_resource.id
    http_method = "ANY"
    authorization = "NONE"
}

resource "aws_api_gateway_integration" "derivative_lambda_integration" {
    rest_api_id = aws_api_gateway_rest_api.api.id
    resource_id = aws_api_gateway_resource.api_resource.id
    http_method = aws_api_gateway_method.proxy_method.http_method
    integration_http_method = "POST"
    type = "AWS_PROXY"
    uri = aws_lambda_function.derivative_lambda.invoke_arn
}

# Allow CORS
resource "aws_api_gateway_method" "options_method" {
    rest_api_id = aws_api_gateway_rest_api.api.id
    resource_id = aws_api_gateway_resource.api_resource.id
    http_method = "OPTIONS"
    authorization = "NONE"
}

resource "aws_api_gateway_method_response" "options_method_response" {
    rest_api_id = aws_api_gateway_rest_api.api.id
    resource_id = aws_api_gateway_resource.api_resource.id
    http_method = aws_api_gateway_method.options_method.http_method
    status_code = "200"
    response_models = {
        "application/json" = "Empty"
    }
    response_parameters = {
        "method.response.header.Access-Control-Allow-Headers" = true,
        "method.response.header.Access-Control-Allow-Methods" = true,
        "method.response.header.Access-Control-Allow-Origin" = true
    }
    depends_on = [aws_api_gateway_method.options_method]
}

resource "aws_api_gateway_integration" "options_integration" {
    rest_api_id = aws_api_gateway_rest_api.api.id
    resource_id = aws_api_gateway_resource.api_resource.id
    http_method = aws_api_gateway_method.options_method.http_method
    type = "MOCK"
    depends_on = [aws_api_gateway_method.options_method]
}

resource "aws_api_gateway_integration_response" "options_integration_response" {
    rest_api_id = aws_api_gateway_rest_api.api.id
    resource_id = aws_api_gateway_resource.api_resource.id
    http_method = aws_api_gateway_method.options_method.http_method
    status_code = aws_api_gateway_method_response.options_method_response.status_code
    response_parameters = {
        "method.response.header.Access-Control-Allow-Headers" = "'Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token'",
        "method.response.header.Access-Control-Allow-Methods" = "'GET,OPTIONS,POST,PUT'",
        "method.response.header.Access-Control-Allow-Origin" = "'*'"
    }
    depends_on = [aws_api_gateway_method_response.options_method_response]
}

resource "aws_api_gateway_deployment" "api_deployment" {
    rest_api_id = aws_api_gateway_rest_api.api.id
    stage_name = "dev"
    depends_on = [
        aws_api_gateway_integration.derivative_lambda_integration,
        aws_api_gateway_integration.options_integration
    ]
}

# Lambda
resource "aws_lambda_permission" "apigw_lambda" {
    statement_id = "AllowExecutionFromAPIGateway"
    action = "lambda:InvokeFunction"
    function_name = aws_lambda_function.derivative_lambda.function_name
    principal = "apigateway.amazonaws.com"
    source_arn = "${aws_api_gateway_rest_api.api.execution_arn}/*/*"
}

resource "aws_lambda_function" "derivative_lambda" {
    function_name = "derivative-telegram-bot"
    filename = "../build/function.zip"
    handler = "src/Lambda.handler"
    role = aws_iam_role.lambda_role.arn
    runtime = "provided"
    source_code_hash = filebase64sha256("../build/function.zip")

    environment {
        variables = {
            TELEGRAM_BOT_TOKEN = var.telegram_bot_token
        }
    }
}

resource "aws_iam_role" "lambda_role" {
    name = "derviative-lambda-role"
    assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow"
    }
  ]
}
EOF
}
