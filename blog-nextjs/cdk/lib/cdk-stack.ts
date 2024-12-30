import * as acm from "aws-cdk-lib/aws-certificatemanager";
import * as cdk from "aws-cdk-lib";
import * as cloudfront from "aws-cdk-lib/aws-cloudfront";
import * as origins from "aws-cdk-lib/aws-cloudfront-origins";
import * as s3 from "aws-cdk-lib/aws-s3";
import * as s3deploy from "aws-cdk-lib/aws-s3-deployment";
import { Construct } from "constructs";

const ODONE_IO = "odone.io";
const ODONE_ME = "odone.me";
const ASSETS_PATH = "../out";

export class CdkStack extends cdk.Stack {
  constructor(scope: Construct, id: string, props?: cdk.StackProps) {
    super(scope, id, props);

    const bucket = new s3.Bucket(this, `${ODONE_IO}-s3-bucket`, {
      bucketName: ODONE_IO,
      versioned: true,
      removalPolicy: cdk.RemovalPolicy.RETAIN,
    });

    const certificate = new acm.Certificate(
      this,
      `${ODONE_IO}-acm-certificate`,
      {
        domainName: ODONE_IO,
        subjectAlternativeNames: [
          `www.${ODONE_IO}`,
          ODONE_ME,
          `www.${ODONE_ME}`,
        ],
        validation: acm.CertificateValidation.fromDns(),
      }
    );

    const cloudfrontFunction = new cloudfront.Function(
      this,
      `${ODONE_IO}-cloudfront-function`,
      {
        code: cloudfront.FunctionCode.fromFile({
          filePath: "lib/cloudfrontFunction.js",
        }),
      }
    );

    const distribution = new cloudfront.Distribution(
      this,
      `${ODONE_IO}-cloudfront-distribution`,
      {
        priceClass: cloudfront.PriceClass.PRICE_CLASS_100,
        certificate,
        domainNames: [ODONE_IO, `www.${ODONE_IO}`, ODONE_ME, `www.${ODONE_ME}`],
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

    new s3deploy.BucketDeployment(this, `${ODONE_IO}-s3-deploy`, {
      sources: [s3deploy.Source.asset(ASSETS_PATH)],
      destinationBucket: bucket,
      distribution,
    });

    new cdk.CfnOutput(this, "cloudfront-distribution-domain", {
      value: distribution.domainName,
    });
  }
}
