import * as acm from "aws-cdk-lib/aws-certificatemanager";
import * as cdk from "aws-cdk-lib";
import * as cloudfront from "aws-cdk-lib/aws-cloudfront";
import * as origins from "aws-cdk-lib/aws-cloudfront-origins";
import * as s3 from "aws-cdk-lib/aws-s3";
import * as s3deploy from "aws-cdk-lib/aws-s3-deployment";
import * as route53 from "aws-cdk-lib/aws-route53";
import * as route53Targets from "aws-cdk-lib/aws-route53-targets";
import { Construct } from "constructs";

const DOMAIN_NAME = "typescript.tips";
const ASSETS_PATH = "../dist";

export class CdkStack extends cdk.Stack {
  constructor(scope: Construct, id: string, props?: cdk.StackProps) {
    super(scope, id, props);

    const bucket = new s3.Bucket(this, `${DOMAIN_NAME}-s3-bucket`, {
      bucketName: DOMAIN_NAME,
      versioned: true,
      removalPolicy: cdk.RemovalPolicy.RETAIN,
    });

    const hostedZone = new route53.PublicHostedZone(
      this,
      `${DOMAIN_NAME}-route53-hosted-zone`,
      {
        zoneName: DOMAIN_NAME,
      }
    );

    const certificate = new acm.Certificate(
      this,
      `${DOMAIN_NAME}-acm-certificate`,
      {
        domainName: DOMAIN_NAME,
        subjectAlternativeNames: [`www.${DOMAIN_NAME}`],
        validation: acm.CertificateValidation.fromDns(hostedZone),
      }
    );

    const cloudfrontFunction = new cloudfront.Function(
      this,
      `${DOMAIN_NAME}-cloudfront-function`,
      {
        code: cloudfront.FunctionCode.fromFile({
          filePath: "lib/cloudfrontFunction.js",
        }),
      }
    );

    const distribution = new cloudfront.Distribution(
      this,
      `${DOMAIN_NAME}-cloudfront-distribution`,
      {
        priceClass: cloudfront.PriceClass.PRICE_CLASS_100,
        certificate,
        domainNames: [DOMAIN_NAME, `www.${DOMAIN_NAME}`],
        defaultBehavior: {
          viewerProtocolPolicy:
            cloudfront.ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
          origin: new origins.S3Origin(bucket),
          functionAssociations: [
            {
              function: cloudfrontFunction,
              eventType: cloudfront.FunctionEventType.VIEWER_REQUEST,
            },
          ],
        },
      }
    );

    new route53.ARecord(this, `${DOMAIN_NAME}-route53-a-record`, {
      zone: hostedZone,
      recordName: DOMAIN_NAME,
      target: route53.RecordTarget.fromAlias(
        new route53Targets.CloudFrontTarget(distribution)
      ),
    });

    new route53.ARecord(this, `${DOMAIN_NAME}-route53-www-a-record`, {
      zone: hostedZone,
      recordName: `www.${DOMAIN_NAME}`,
      target: route53.RecordTarget.fromAlias(
        new route53Targets.CloudFrontTarget(distribution)
      ),
    });

    new route53.TxtRecord(
      this,
      `${DOMAIN_NAME}-route53-google-site-verification-record`,
      {
        zone: hostedZone,
        values: [
          `google-site-verification=b-jdrVZdvHX9m3CA2e_cx02q46c-UMGu-dwATLrp9xI`,
        ],
      }
    );

    new s3deploy.BucketDeployment(this, `${DOMAIN_NAME}-s3-deploy`, {
      sources: [s3deploy.Source.asset(ASSETS_PATH)],
      destinationBucket: bucket,
      distribution,
    });

    new cdk.CfnOutput(this, "cloudfront-distribution-domain", {
      value: distribution.domainName,
    });
  }
}
